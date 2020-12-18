identity : ty -> ty
identity x = x

double : Num ty => ty -> ty
double x = x + x
