
struct list_short;
struct list_int;
struct list_long;
struct list_long_long;

list_short*         cons_short(short x, list_short* xs);

short               head_short(list_short* xs);
int                 head_int(list_int* xs);
long                head_long(list_long* xs);
long long           head_long_long(list_long_long* xs);
unsigned short      head_unsigned_short(list* xs);
unsigned int        head_unsigned_int(list* xs);
unsigned long       head_unsigned_long(list* xs);
unsigned long long  head_unsigned_long_long(list* xs);