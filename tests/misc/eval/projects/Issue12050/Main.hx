function main() {}

function toString<T>(status:BroadcastStatus<T>)
	return switch status {
		case NotAvailable: 'NotAvailable';
	}

interface MediaDevice {}

enum BroadcastStatus<T:MediaDevice> {
	NotAvailable;
}
