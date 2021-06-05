

constructorX(Nombre, Apellido, Nacionalidad, [DD,MM,AAAA], Salida):-
    string(Nombre),
    string(Apellido),
    string(Nacionalidad),
    integer(DD),
    integer(MM),
    integer(AAAA),
    Salida = [Nombre, Apellido, Nacionalidad, [DD,MM,AAAA]].

constructorY(Nombre, Apellido, Nacionalidad, [DD,MM,AAAA], [Nombre, Apellido, Nacionalidad, [DD,MM,AAAA], []]):-
    string(Nombre),
    string(Apellido),
    string(Nacionalidad),
    integer(DD),
    integer(MM),
    integer(AAAA).

isSN(Nombre, Fecha, [ListUserA_X|ListUserA_Y], ListUser, ListPub,[Nombre, Fecha, [ListUserA_X|ListUserA_Y], ListUser, ListPub]):-
    string(Nombre),
    isDate(Fecha),
    string(ListUserA_X),
    string(ListUserA_Y),
    isListUser(ListUser),
    isListPub(ListPub).
