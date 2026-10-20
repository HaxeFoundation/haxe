# Compilation incrémentale : plan d'implémentation

Objectif : que le serveur de compilation ne retype, ne post-traite et ne régénère
que ce qui a réellement changé, avec un résultat **identique bit à bit** à une
compilation à froid.

## 0. Décisions retenues

| Sujet | Décision |
|---|---|
| Unité d'inférence | **Le champ.** Le type d'un champ doit être entièrement déterminé à la fin de son propre typage. Un monomorphe encore ouvert dans le type d'un champ (`static var x = []`) est une **erreur** : annotation requise. |
| Granularité | **Le champ**, directement : dépendances, empreintes et retypage par champ. |
| Périmètre | Typage, filtres et génération. |
| Cibles prioritaires | **JS** et **HL**. Les autres cibles gardent une génération globale, mais profitent du typage et des filtres incrémentaux. |
| Macros | Traçage automatique des dépendances, macros *volatiles* ré-exécutées avec arrêt de propagation sur leur sortie, **API d'agrégation** (map/reduce) pour les registres, warning sur les écritures statiques non agrégées. |
| Compatibilité | Rupture assumée pour Haxe 5, avec une phase de warnings avant les erreurs. |
| Stockage | **En mémoire**, en gardant l'identité physique des objets. hxb n'est pas sur le chemin critique ; il reste un format de persistance et un outil de vérification. |
| Positions dans le code inliné | L'inliner remplace les positions par celle du site d'appel (`map_pos`, `src/optimization/inline.ml`) : elles sortent de l'empreinte. Exceptions : `-D keep_inline_positions` (déjà dans la signature des defines, on inclut alors les positions relatives dans l'empreinte du corps) et les closures auto-appelantes (positions relatives incluses). |
| Corpus de validation | Tests du dépôt (`tests/unit`, `tests/server`, `tests/display`) ; Heaps, hxbit et domkit, pour leur usage intensif des macros. |

## 1. État actuel (point de départ)

- Le cache est **par module** (`module_def_extra`, `src/core/tType.ml`). Chaque
  dépendance (`m_deps : (int, module_dep) PMap.t`) indique seulement *de quel
  module* on dépend, jamais *de quoi* (interface, corps inline, macro…).
- `check_module` (`src/compiler/server/serverCache.ml`) invalide de façon
  **transitive et aveugle** (`DependencyDirty`) : modifier le corps d'une
  fonction privée d'un module de base invalide tout le graphe au-dessus.
- La détection de changement se fait sur la date de modification, ou sur l'AST
  parsé complet (`CheckFileContentModification`), qui inclut les corps.
- Les monomorphes ouverts sont fermés à `Any` au moment de la sauvegarde
  (`update_cache_dependencies ~close_monomorphs:true`, `src/filters/filters.ml`).
  Le typage dépend donc déjà de l'état du cache.
- Le post-traitement est global : `Filters.run` travaille sur `com.types`. DCE,
  `RenameVars`, `add_descendant`, `has_feature` et la pureté de l'analyzer
  portent sur tout le programme. Les filtres modifient l'AST typé **en place**,
  puis `save_class_state` et `cl_restore` le restaurent.
- Les générateurs régénèrent toute la sortie. `genhl` maintient des tables
  globales d'indices (`cglobals`, `cfunctions`, `cstrings`, `cached_types`…).

## 2. Principes

1. **Graphe de requêtes avec arrêt de propagation.** Chaque nœud produit un
   résultat et une **empreinte**. Chaque dépendance enregistre `(nœud, empreinte
   vue)`. Valider un nœud, c'est revérifier ses dépendances ; on ne le
   ré-exécute que si l'une d'elles a changé. Si un nœud ré-exécuté produit la
   même empreinte, ses dépendants ne sont pas touchés.
2. **Séparer l'interface de l'implémentation**, par champ et par type.
3. **Identité physique stable** de toutes les entités qu'un autre module peut
   référencer. Retyper un champ modifie le contenu de l'objet existant ; on ne
   le remplace jamais par un autre objet.
4. **L'AST typé est immuable une fois produit.** Le post-traitement produit une
   copie, uniquement pour ce qui a changé.
5. **Déterminisme** : aucun résultat ne doit dépendre de l'ordre de typage. Un
   mode de vérification compare systématiquement l'incrémental et le froid.

## 3. Modèle de données

### 3.1 Entités

Une *entité* est un objet qu'un autre module peut référencer par pointeur :

- `module_def` (son `m_id` est stable entre les compilations) ;
- les types de module : `tclass`, `tenum`, `tdef`, `tabstract`, ainsi que
  `m_statics` et `a_impl` ;
- les champs : `tclass_field` (y compris les overloads et le constructeur),
  `tenum_field` ;
- les paramètres de type : `typed_type_param` et leur classe `KTypeParameter` ;
- les `tanon` déclarés par des typedefs.

Chaque entité reçoit une **clé stable** indépendante de l'adresse mémoire :
`(chemin de module, chemin du type, genre, nom, index d'overload | index de paramètre)`.
La correspondance ancien ↔ neuf entre deux typages passe par cette clé.

### 3.2 Nœuds et empreintes

| Nœud | Contenu | Empreinte |
|---|---|---|
| `File(f)` | contenu du fichier | hash du contenu (la date ne sert que de pré-filtre) |
| `Decls(m)` | AST parsé, découpé par type et par champ | hash de l'AST **sans positions absolues**, par champ |
| `TypeShape(t)` | chemin, genre, paramètres, contraintes, `super`/`implements`, **ensemble des membres** (noms, genres, accès), métadonnées, `@:using`, sous-type d'abstract, `from`/`to` | hash canonique |
| `FieldSig(cf)` | type entièrement résolu, genre (`var`/`method`, accès), `cf_params`, métadonnées, flags pertinents | hash canonique |
| `FieldInlineBody(cf)` | corps des champs `inline`, `macro`, `@:generic`, ou de champs d'abstract utilisés comme casts/opérateurs inline | hash canonique du `texpr` |
| `FieldBody(cf)` | `cf_expr` typé | hash canonique du `texpr` (sert au post-traitement et à la génération) |
| `MacroExpansion(site)` | résultat d'une expansion et ses effets | hash de la sortie |
| `PostProcessed(cf)` | expression après filtres | hash |
| `Fragment(t)` | code généré pour un type ou une fonction | hash |

**Hasher canonique** (nouveau module `src/core/tFingerprint.ml`) :
- les types de module sont identifiés par leur chemin, jamais par leur adresse ;
- les paramètres de type par `(hôte, index)` ;
- les champs d'anonymes sont triés par nom ;
- les monomorphes liés sont suivis ; un monomorphe non lié est interdit dans
  une signature, ce que garantit la phase 1 ;
- les `TLazy` sont forcés avant le hachage (ils l'ont déjà été par `PForce`) ;
- les positions sont exclues, sauf positions relatives dans les cas listés au §0 ;
- il écrit directement dans un digest incrémental, sans sérialisation
  intermédiaire.

### 3.3 Dépendances

Le *consommateur* est le nœud en cours de calcul, en général le champ en cours
de typage (`ctx.f.curfield`), ou une expansion de macro. Types de dépendance :

| Type | Enregistré quand | Invalide si |
|---|---|---|
| `DShape t` | itération ou recherche dans les membres de `t`, unification structurelle, `using`, exhaustivité du matcher, vérification des overrides, résolution d'overloads | `TypeShape(t)` change |
| `DSig cf` | lecture de `cf.cf_type` (accès à un champ, appel) | `FieldSig(cf)` change |
| `DInline cf` | inlining, instanciation `@:generic`, exécution d'une macro (contexte macro) | `FieldInlineBody(cf)` ou `FieldBody(cf)` change |
| `DLookup (path, résultat)` | résolution d'un nom de type, d'un import, d'un `import pack.*`, sous-types | le résultat de la résolution change, y compris un fichier qui **apparaît** et masque un autre type (dépendance négative) |
| `DMacro site` | utilisation d'une expansion de macro | `MacroExpansion(site)` change |
| `DFile f` | lecture de fichier par une macro, `registerModuleDependency` | `File(f)` change |
| `DDefine` | déjà couvert par la signature des defines (`m_sign`) | |

Stockage : `m_deps` reste comme résumé au niveau module, pour les outils
existants et le diagnostic. Le nouveau graphe vit dans une structure à part,
`IncrementalGraph`, rattachée au contexte de cache (`CompilationCache`) :

```ocaml
type node_key =
  | NFile of Path.UniqueKey.t
  | NDecls of path                       (* module *)
  | NShape of entity_key
  | NSig of entity_key
  | NInline of entity_key
  | NBody of entity_key
  | NMacro of macro_site_key
  | NPost of entity_key
  | NFragment of target * entity_key

type node = {
  key : node_key;
  mutable fingerprint : Digest.t;
  mutable deps : (node_key * Digest.t) list;   (* ce qu'on a lu, et l'empreinte vue *)
  mutable rdeps : node_key list;               (* index inverse, pour le diagnostic et la génération *)
  mutable verified_at : int;                   (* étape de compilation *)
  mutable state : Green | Red | Unknown;
}
```

## 4. Phases

Chaque phase est livrable et mesurable seule. Les phases 0 et 1 n'ont pas de
dépendance entre elles et peuvent démarrer en parallèle.

---

### Phase 0 : infrastructure de vérification et de mesure

But : pouvoir prouver l'équivalence avant d'optimiser quoi que ce soit.

1. **Mode de vérification** `-D incremental-verify` : après une compilation
   serveur, on refait une compilation à froid dans un contexte vierge, puis on
   compare :
   - les empreintes de tous les nœuds `Sig`, `Shape` et `Body` ;
   - les sorties générées, octet par octet ;
   - la liste des erreurs et warnings, triée.
2. **Vérificateur de pointeurs périmés** `-D incremental-check-refs` : on
   parcourt tous les modules en cache et on vérifie que chaque référence vers
   une entité du module `X` (dans les types, les `texpr`, `cl_super`,
   `a_from_field`…) désigne un objet atteignable depuis `X.m_types`.
3. **Outil de rejeu d'historique** (`tests/incremental/`) : un harnais Haxe qui
   lance un serveur, prend un dépôt et une liste de commits, compile chaque
   commit de façon incrémentale avec le mode de vérification, et produit un
   rapport (temps, nœuds recalculés, différences).
   - Corpus : `tests/unit`, `tests/server`, `tests/display` ; Heaps, hxbit et
     domkit (dépôts clonés en CI, avec des commits rejoués).
4. **Statistiques** (`--times` et `server/stats`) : nœuds validés, recalculés et
   coupés par l'arrêt de propagation, par type de nœud.

Critère de sortie : le harnais tourne en CI sur le serveur actuel
(granularité module) et ne remonte aucune différence. On obtient ainsi une
référence de temps.

---

### Phase 1 : règles sémantiques (rupture Haxe 5)

But : rendre l'inférence locale au champ.

1. **Détection.** À la fin du typage d'un champ (après résolution du `TLazy`
   posé par `typeloadFields.ml`, lorsque `cf_type` est final), on parcourt
   `cf_type` à la recherche d'un `TMono` non lié ou d'un `TAnon` ouvert
   (`a_status` modifiable).
   - Phase 1a : warning `WUnresolvedFieldType` : « Le type de `x` n'est pas
     entièrement inféré, ajoutez une annotation ».
   - Phase 1b : erreur par défaut. `-D legacy-field-inference` rétablit le
     warning pour une version.
2. **Instrumentation des liaisons.** Dans `Monomorph.do_bind`
   (`src/core/tUnification.ml`), si le monomorphe appartient à la signature
   d'un **autre** champ que celui en cours de typage, on émet un
   diagnostic : c'est le cas qui disparaît avec la règle. Cela sert à mesurer
   l'impact sur le corpus avant de passer à l'erreur.
3. **Suppression** de `close_monomorphs` dans `update_cache_dependencies`, qui
   devient inutile, et de l'erreur « Unclosed monomorph » côté hxb, qui devient
   un `die`.
4. **Mise à jour** de la stdlib, des tests et des bibliothèques du corpus
   (annotations ajoutées).

Cas à traiter explicitement :
- `var x = null;` et `var a = [];` : erreur.
- `function f() return g();` où `g` n'est pas encore typé : autorisé ; le type
  de `f` dépend de `FieldSig(g)` (typage paresseux, comme aujourd'hui).
- Cycles de champs sans annotation (`f` appelle `g`, `g` appelle `f`) : on
  garde le comportement actuel (« recursive type »), mais on enregistre le
  cycle comme une composante pour l'invalidation.
- Locales et closures : non concernées, elles sont internes au corps.

Critère de sortie : toute la CI est verte avec l'erreur activée, et l'impact sur
le corpus est documenté.

---

### Phase 2 : identité stable et séparation du contenu

But : pouvoir modifier une entité en place sans casser les pointeurs des autres
modules.

1. **Clés d'entités** (§3.1) et index `entity_key -> objet` par module, dans
   `CompilationCache`.
2. **Rendre mutables** `cf_pos`, `cf_name_pos`, `ef_pos`, `mt_pos`/`cl_pos`,
   `e_pos`, `t_pos`, `a_pos` et les `*_name_pos` (`src/core/tType.ml`).
   `ef_index` reste immuable : un changement d'index est un changement de forme.
3. **Contributions externes : les sortir des entités, ou les rendre
   recalculables.**
   - `cl_descendants` : recalculé par la passe d'agrégation (phase 7) à partir
     des `cl_super` et `cl_implements` de tous les types ; il n'est plus stocké
     de façon persistante.
   - Flags DCE et post-traitement (`CfUsed`, `CfPostProcessed`, …) : déplacés
     dans les données de post-traitement (phase 7), plus dans `cf_flags`.
   - `m_features` : calculé par module à partir de son AST post-traité.
   - Métadonnées ajoutées par une macro d'un **autre** module (`addMetadata`) :
     enregistrées comme effet de l'expansion (phase 6) et rejouées.
   - Instances `@:generic` : rattachées au nœud `(générique, arguments de type)`
     et non au module générique (supprime la mécanique `m_processed = 0` de
     #10635).
4. **Greffe** (`src/compiler/incremental/graft.ml`) :
   - entrée : l'ancien et le nouvel ensemble d'entités d'un module ;
   - on apparie les entités par clé. Si l'appariement échoue (entité ajoutée ou
     supprimée), c'est un changement de forme : on garde les objets appariés et
     on ajoute ou retire les autres ;
   - on copie le contenu neuf dans les anciens objets ;
   - on **remappe** neuf → ancien dans les types et `texpr` du module
     (`TField`, `TTypeExpr`, `TInst`, paramètres de type…) ;
   - les objets neufs non appariés deviennent les nouvelles entités.
5. Aucune `tclass` n'est plus recréée pour un module en cache : `typeloadModule`
   prend un « module précédent » optionnel et réutilise ses entités par clé
   (via la greffe).

Critère de sortie : en retypant intégralement un module modifié avec greffe, le
vérificateur de pointeurs ne remonte rien sur tout le corpus.

---

### Phase 3 : empreintes

1. `src/core/tFingerprint.ml` : hasher canonique (§3.2) pour `t`, `tclass_field`,
   la forme d'un type et `texpr`.
2. Calcul de `FieldSig` et `FieldInlineBody` à la fin du typage de chaque champ ;
   de `TypeShape` à la fin de `PBuildClass` et `PConnectField` ; de `FieldBody`
   à la fin de `PTypeField`.
3. Hash de `Decls` **par champ** dans `TypeloadParse` : on découpe l'AST par
   champ et on calcule des positions relatives, ce qui permet de savoir quels
   champs ont changé sans les typer.
4. Tests unitaires du hasher : stabilité (deux typages à froid donnent la même
   empreinte), sensibilité (chaque modification de signature change
   l'empreinte), insensibilité aux positions.

---

### Phase 4 : enregistrement des dépendances

Points d'instrumentation (tous passent par un `Incremental.record dep`, qui ne
fait rien hors du serveur) :

| Dépendance | Où |
|---|---|
| `DLookup` | `Typeload.load_instance`/`load_type_def`, résolution d'imports (`ImportHandling`, `typeloadModule.ml`), `TyperDotPath`, recherche dans les classpaths (y compris l'**échec** de recherche) |
| `DSig` | `FieldAccess`/`Fields` (accès à un champ), `Calls`, `CallUnification` ; lecture de `cf_type` des champs d'autres modules |
| `DShape` | recherche de champ (`Fields.type_field`, `class_field`), `using`, unification structurelle (`tUnification.ml`, `TAnon` contre `TInst`), `OverloadResolution`, `Matcher` (constructeurs d'enum), vérification des overrides et des interfaces (`TypeloadCheck`), `AbstractCast` (`from`/`to`) |
| `DInline` | `Inline`, `Generic`, `MacroContext` (code exécuté) |
| `DMacro`, `DFile` | phase 6 |

Le consommateur courant est porté par le contexte du typer
(`ctx.f.curfield` et son module). Les dépendances d'un champ vers son propre
module sont aussi enregistrées, car la granularité est le champ.

Optimisation : les dépendances sont dédupliquées par consommateur, et les
lectures dans le module propre via `DShape` sont regroupées.

Critère de sortie : le mode de vérification, en mode « tracer seulement »
(on recalcule tout mais on compare avec ce que le graphe aurait invalidé),
montre qu'**aucun nœud qui a changé n'aurait été considéré comme valide**.
C'est le test qui prouve que les dépendances sont complètes.

---

### Phase 5 : validation et retypage par champ (cœur du serveur)

Remplace `check_module` et `type_module` (`serverCache.ml`).

1. **Validation rouge/verte** à la demande : `validate node` valide d'abord
   récursivement ses dépendances et compare les empreintes vues. Si tout est
   vert, le nœud est vert ; sinon on le recalcule, puis on compare sa nouvelle
   empreinte à l'ancienne.
2. **Fichier modifié** :
   1. reparse ; calcul des empreintes `Decls` par champ et par type ;
   2. types ajoutés ou supprimés, et membres ajoutés ou supprimés : la
      `TypeShape` change ;
   3. champs dont l'empreinte `Decls` a changé : retypage de ces seuls champs,
      dans l'entité existante (on réinitialise `cf_type` et `cf_expr`, on
      relance le typage du champ, puis on greffe) ;
   4. les build macros du type sont ré-exécutées si leur nœud `MacroExpansion`
      est invalide. Leur sortie est diffée par champ comme au point 2.3.
3. **Arrêt de propagation** : si `FieldSig(cf)` est inchangé après retypage, les
   consommateurs de `DSig cf` restent verts. Seuls `FieldBody(cf)`, puis le
   post-traitement et la génération de `cf`, sont recalculés.
4. **Typage d'un module à la demande** (`Typeload` qui demande un module) : si
   le module est en cache, on valide les nœuds `Shape` et `Sig` nécessaires
   plutôt que tout le module. Le typage paresseux des champs (`TLazy`) devient
   « valider `FieldSig(cf)` ».
5. **Display et complétion** : le fichier affiché est toujours retypé
   (`AllowPartialTyping` actuel), mais il bénéficie de l'arrêt de propagation
   pour tout le reste.
6. **Erreurs et warnings** : stockés par nœud et rejoués lorsque le nœud est
   vert, ce qui remplace la logique actuelle des modules en erreur.
7. **Macros d'initialisation** (`--macro`) et callbacks globaux : toujours
   exécutés, en dernier, avec arrêt de propagation sur ce qu'ils produisent
   (phase 6).

Critère de sortie : équivalence sur tout le corpus en rejeu d'historique.
Mesure : nombre de champs retypés par commit, temps de typage.

---

### Phase 6 : macros

1. **Clé d'expansion** :
   - macros d'expression : `(champ macro appelé, hash des arguments AST, champ consommateur, index de l'appel dans le corps)` ;
   - build macros : `(type construit, macro, hash des champs d'entrée)`.
2. **Traçage** dans `src/macro/macroApi.ml` : chaque fonction de l'API qui lit
   le programme enregistre une dépendance pour l'expansion courante
   (`getType` donne `DLookup` + `DShape`, `typeof`/`typeExpr` les dépendances du
   typage déclenché, `getBuildFields` et `getLocalClass` le contexte, etc.).
3. **Primitives sys de l'interpréteur** (`src/macro/eval/evalStdLib.ml`) :
   - `File.getContent`/`getBytes`, `FileSystem.exists`/`stat`/`readDirectory` :
     `DFile` (contenu, ou liste triée pour un répertoire) ;
   - `Date.now`, `Math.random`, `Sys.getEnv`, `Sys.command`, `Sys.time`,
     sockets, écriture de fichiers : l'expansion est marquée **volatile**.
4. **Écritures statiques** : l'interpréteur eval instrumente l'écriture d'un
   champ statique d'une classe du contexte macro **pendant** une expansion.
   L'expansion est alors marquée volatile, et on émet un warning
   `WMacroStaticState` (« cette macro modifie un état statique : utilisez
   l'API d'agrégation pour qu'elle soit incrémentale »).
5. **Effets enregistrés et rejoués** : `defineType`, `defineModule`,
   `addMetadata`, `addResource`, `registerModuleDependency`, les callbacks
   (`onGenerate`, `onAfterTyping`, `onTypeNotFound`) et les messages sont
   journalisés par expansion. Quand l'expansion est verte, on rejoue le journal
   au lieu d'exécuter la macro.
6. **Macros volatiles** : ré-exécutées à chaque compilation. Si leur sortie
   (et leur journal d'effets) a la même empreinte, l'aval reste vert.
7. **Code de la macro** : l'expansion dépend de `DInline` (corps) sur toutes les
   fonctions du contexte macro qu'elle exécute. Version simple pour commencer :
   dépendance sur le corps de tous les modules du contexte macro atteints par
   l'exécution, enregistrés par l'interpréteur.
8. **API d'agrégation** (nouvelle, `haxe.macro.Context` ou
   `haxe.macro.Aggregate`) :

   ```haxe
   // Contribution, mise en cache avec le nœud qui l'émet (un type, une expansion).
   Aggregate.contribute("hxbit.serializable", macro $v{cl.name});
   // Réduction : exécutée à chaque compilation sur toutes les contributions
   // (en cache et nouvelles), avec arrêt de propagation sur son résultat.
   Aggregate.reduce("hxbit.serializable", (all:Array<Expr>) -> defineRegistryType(all));
   ```

   Migration de hxbit (registre des classes `Serializable`), domkit
   (composants) et Heaps (registres de shaders, de ressources) pendant la
   phase, pour valider l'API sur des cas réels.

Critère de sortie : Heaps, hxbit et domkit passent en rejeu d'historique avec
au plus une poignée de macros volatiles identifiées et justifiées.

---

### Phase 7 : filtres incrémentaux

1. **Copie post-traitée** : les filtres travaillent sur une copie de
   `cf_expr` (`PostProcessed(cf)`) et ne modifient plus l'AST typé.
   `save_class_state`, `cl_restore` et `cf_expr_unoptimized` disparaissent :
   l'AST typé reste la source et sert pour l'inlining suivant.
2. **Filtres locaux**, qui ne dépendent que du champ et des interfaces : mis en
   cache par `PostProcessed(cf)`, avec comme dépendances `FieldBody(cf)`, les
   `DSig`/`DShape` lus et la configuration de plateforme. Concerne la majorité
   de `run_safe_filters` et `destruction_on_scom`.
3. **Passes globales, réécrites sur des résumés par champ** :
   - **DCE** (`src/optimization/dce.ml`) : chaque champ post-traité produit un
     résumé « champs et types référencés, features utilisées, `@:keep` ».
     L'atteignabilité est un graphe maintenu de façon incrémentale : on ajoute
     ou retire les arêtes des champs modifiés, puis on propage les différences
     (marquer ou démarquer). Version initiale acceptable : recalcul complet de
     l'atteignabilité sur les résumés, très rapide car sans parcours d'AST.
   - **Descendants** (`add_descendant`) : recalculés depuis les formes.
   - **Features** (`has_feature`) : union des features des champs atteignables.
   - **`RenameVars`** : la partie globale (noms réservés et collisions) est
     calculée sur les résumés. Le renommage par fonction est local, donc mis en
     cache.
   - **Pureté (analyzer)** : point fixe calculé sur le graphe d'appels des
     résumés, par composante fortement connexe. Seules les composantes
     touchées sont recalculées.
4. Sortie de la phase : pour chaque champ atteignable, un `PostProcessed` et un
   ensemble global (types gardés, features, descendants) avec leur empreinte.

---

### Phase 8 : génération JS

`src/generators/genjs.ml` est découpé en :

1. **Fragment par type** : le code JS d'une classe, d'un enum ou d'un abstract
   implémenté, plus ses métadonnées (dépendances d'initialisation statique,
   `__init__`, features utilisées). Il est mis en cache par
   `Fragment(js, type)`, avec comme dépendances les `PostProcessed` de ses
   champs, les noms natifs référencés et l'ensemble global (features).
2. **Sourcemap par fragment**, avec des positions relatives au fragment.
3. **Édition de liens** : en-tête, ordre des types (dépendances d'héritage),
   initialisation des statiques, concaténation, décalage des sourcemaps.
   Linéaire en la taille de la sortie, sans aucun parcours d'AST.
4. Écriture du fichier seulement si le contenu change (à partager avec les
   autres cibles).

Critère de sortie : sortie identique octet par octet au mode froid
(`tests/unit` en JS, sourcemaps comprises).

---

### Phase 9 : génération HL

C'est la partie la plus lourde, à cause des tables globales de `genhl.ml`
(`cglobals`, `cstrings`, `cfunctions`, `cnatives`, `cached_types`,
`anons_cache`, `method_wrappers`…).

1. **Compilation par fonction avec références symboliques** : chaque fonction
   est compilée en un `fundecl` dont les opérandes globaux sont des symboles
   (`SymString "foo"`, `SymGlobal path`, `SymFun (path, field)`,
   `SymType hl_type_key`) plutôt que des indices.
2. **Types HL** : `hl_type_key` canonique et indépendant de l'ordre (aujourd'hui,
   le cache des types dépend de l'ordre de rencontre). Les types de classes et
   les objets virtuels (anonymes) sont dérivés de `TypeShape` et `FieldSig`.
3. **`hlopt` par fonction** : optimisation mise en cache avec le fragment
   (le résultat ne dépend que de la fonction symbolique).
4. **Éditeur de liens HL** : attribue les indices (types, globaux, chaînes,
   fonctions, natives) dans un **ordre déterministe** (tri par clé), résout les
   symboles et écrit le bytecode et les informations de debug. Sa complexité est
   linéaire en la taille du code.
5. **HL/C (`hl2c`)** : génère un fichier C par type à partir des fragments liés,
   et n'écrit que les fichiers modifiés (ce qui évite de recompiler en C).
6. Risque : l'ordre des indices change la sortie binaire par rapport au mode
   actuel. On accepte un nouveau format déterministe, et l'équivalence se
   vérifie entre incrémental et froid **après** la phase.

Critère de sortie : `tests/unit` en HL (JIT et HL/C) passe, avec une sortie
identique entre l'incrémental et le froid.

---

### Phase 10 : nettoyage

- Suppression de l'invalidation au niveau module : `DependencyDirty`,
  `m_checked`, `m_cache_state` (ou réduction à un résumé de diagnostic),
  `m_processed`.
- hxb : utilisé seulement pour la persistance sur disque (redémarrage du
  serveur) et dans le mode de vérification. Le chargement depuis hxb doit
  restaurer les empreintes et le graphe.
- Documentation utilisateur : nouvelles erreurs (types de champs), API
  d'agrégation, warnings sur les macros, defines `incremental-verify` et
  `incremental-check-refs`.

## 5. Ordre et dépendances entre phases

```
Phase 0 ─┬─────────────────────────────────────────────┐
Phase 1 ─┤                                             │
         ├─ Phase 2 ─ Phase 3 ─ Phase 4 ─ Phase 5 ─┬─ Phase 7 ─┬─ Phase 8 (JS)
         │                                         │           └─ Phase 9 (HL)
         │                           Phase 6 ──────┘
         └─ (vérification en continu sur tout le corpus)       Phase 10
```

La phase 6 (macros) démarre après la phase 4 (même mécanique
d'enregistrement) et doit être terminée avant d'activer la phase 5 sur Heaps,
hxbit et domkit.

## 6. Risques et points ouverts

- **Complétude des dépendances.** C'est le risque principal : une dépendance
  oubliée donne un résultat faux silencieux. Parade : le mode « tracer
  seulement » de la phase 4 et le rejeu d'historique en CI.
- **Coût mémoire** du graphe : estimé à quelques dizaines d'octets par arête.
  À mesurer en phase 4 sur Heaps ; si besoin, compacter par module
  (tableaux d'entiers indexés par entité).
- **Coût de l'enregistrement** pendant le typage à froid : il doit rester
  faible. On le désactive hors serveur.
- **Unification structurelle** : `DShape` est grossier (ajouter un champ privé
  invalide les consommateurs structurels). Affinage possible : forme publique
  et forme privée séparées.
- **Macros qui modifient d'autres types** hors du cadre des effets journalisés
  (mutation directe via l'API `TypedExpr`/`ClassType` d'un objet obtenu par
  `getType`) : à détecter (écriture sur une entité d'un autre module pendant
  une expansion), et à traiter par une invalidation large et un warning.
- **`@:multiType`, `@:genericBuild`, `onTypeNotFound`** : à couvrir
  explicitement dans les tests.
- **Display** : vérifier que la complétion n'est pas ralentie par la
  validation. Si c'est le cas, valider seulement à la demande.
- **Format HL déterministe** : changement visible pour les outils qui lisent le
  bytecode (debugger, profiler), à valider avec eux.

## 7. Tests à ajouter

- `tests/server` : un cas par type de dépendance (modification de la signature,
  du corps, du corps inline, de la forme, fichier qui masque un type, define
  modifié, fichier lu par une macro…), avec contrôle des nœuds recalculés via
  les statistiques du serveur.
- `tests/misc` : erreurs de la phase 1 (types de champs non inférés).
- `tests/incremental` : harnais de rejeu (phase 0) sur `tests/unit`, Heaps,
  hxbit et domkit.
- Tests unitaires du hasher et de la greffe (correspondance ancien ↔ neuf,
  remapping, vérificateur de pointeurs).
