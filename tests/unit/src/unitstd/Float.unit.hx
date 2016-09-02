var nan = Math.NaN;
var pinf = Math.POSITIVE_INFINITY;
var ninf = Math.NEGATIVE_INFINITY;
var fl:Float = 0.0;

fl > nan == false;
fl < nan == false;
fl >= nan == false;
fl <= nan == false;
fl == nan == false;
fl != nan == true;

nan > nan == false;
nan < nan == false;
nan >= nan == false;
nan <= nan == false;
nan == nan == false;
nan != nan == true;

pinf > nan == false;
pinf < nan == false;
pinf >= nan == false;
pinf <= nan == false;
pinf == nan == false;
pinf != nan == true;

ninf > nan == false;
ninf < nan == false;
ninf >= nan == false;
ninf <= nan == false;
ninf == nan == false;
ninf != nan == true;

nan > fl == false;
nan < fl == false;
nan >= fl == false;
nan <= fl == false;
nan == fl == false;
nan != fl == true;

nan > pinf == false;
nan < pinf == false;
nan >= pinf == false;
nan <= pinf == false;
nan == pinf == false;
nan != pinf == true;

nan > ninf == false;
nan < ninf == false;
nan >= ninf == false;
nan <= ninf == false;
nan == ninf == false;
nan != ninf == true;