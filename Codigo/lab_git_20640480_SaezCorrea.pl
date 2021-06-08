/*
HECHOS

usuario(ID, Name, Password, Actividad, Lista Publicaciones, Lista de Publicaciones Compartidas, Fecha creacion, Cantidad Follows, Cantidad de seguidores, Lista de Follows, Lista de seguidores):
    ID                                  -> Entero, Corresponde al ID de la cuenta
    Name                                -> String, Corresponde al nombre de la cuenta
    Password                            -> String, Corresponde a la contrasena de la cuenta
    Actividad                           -> Entero, Corresponde a la actividad de la cuenta
    Lista Publicaciones                 -> Lista, Corresponde a una lista de preguntas señalada por el ID
    Lista de Publicaciones Compartidas  -> Lista, Corresponde a una lista de publicaciones compartidas senalada por el ID
    Fecha Creacion                      -> Lista, Corresponde a la fecha de creacion, es una lista
    Cantidad de Follows                 -> Entero, Corresponde a la cantidad de follows que hay
    Cantidad de seguidores              -> Entero, Corresponde a la cantidad de seguidores que tiene
    Lista de Follows                    -> Lista, Corresponde a los users que lo siguen
    Lista de seguidores                 -> Lista, Corresponde a los users que sigue

Publicaciones(ID,Autor, Fecha publicacion, Tipo de publicacion, Contenido, Lista de Comentarios, Likes, Cantidad de veces compartida):
    ID                              -> Entero, Corresponde al ID de la publicacion
    Autor                           -> String, Corresponde al nombre del autor
    Fecha publicacion               -> Lista, Corresponde a la fecha de publicacion de la publicacion
    Tipo de publicacion             -> String, Corresponde al tipo de contenido que es
    Contenido                       -> String, Corresponde al contenido de la publicacion
    Lista de comentarios            -> Lista, Corresponde a una lista de comentarios segun su ID
    Likes                           -> Entero, Corresponde a los likes de la publicacion
    Cantidad de veces compartida    -> Entero, Corresponde a la cantidad de veces compartida la publicacion


comentario(ID, Autor, Fecha del comentario, Contenido, Likes):
    ID                      -> Entero, Corresponde al ID del comentario
    Autor                   -> String, Corresponde al nombre del autor del comentario
    Fecha del comentario    -> Lista, Corresponde a la fecha que se publico el comentario
    Contenido               -> String, Corresponde al contenido del comentario
    Likes                   -> Entero, Corresponde a la cantidad de likes que tiene el comentario

socialnetwork(Name, Fecha de creacion, Lista de Usuarios, Lista de Publicaciones, Lista de Comentarios):
    Name                    -> String, Corresponde al nombre del socialnetwork
    Fecha de creacion       -> Lista, Corresponde a la fecha de creacion
    Lista de Usuarios       -> Lista, Corresponde a la lista de usuarios en el socialnetwork
    Lista de Publicaciones  -> Lista, Corresponde a la lista de Publicaciones en el socialnetwork
    Lista de Comentarios    -> Lista, Corresponde a la lista de Comentarios en el socialnetwork
*/

%Constructores

usuario(ID, Name, Password, Actividad, ListPub, ListPubComp, Fecha, CantFollow, CantSeg, ListFollow, ListSeg, [ID, Name, Password, Actividad, ListPub, ListPubComp, Fecha, CantFollow, CantSeg, ListFollow, ListSeg]):-
    integer(ID),
    string(Name),
    string(Password),
    integer(Actividad),
    isListaInteger(ListPub),
    isListaComp(ListPubComp),
    isListaInteger(Fecha),
    integer(CantFollow),
    integer(CantSeg),
    isListaString(ListFollow),
    isListaString(ListSeg).

publicacion(ID,Autor, Fecha, Tipo, Contenido, ListComentarios, Likes, CantidadCompartida, [ID,Autor, Fecha, Tipo, Contenido, ListComentarios, Likes, CantidadCompartida]):-
    integer(ID),
    string(Autor),
    isListaInteger(Fecha),
    string(Tipo),
    string(Contenido),
    isListaInteger(ListComentarios),
    integer(Likes),
    integer(CantidadCompartida).

comentario(ID, Autor, Fecha, Contenido, Likes, [ID, Autor, Fecha, Contenido, Likes]):-
    integer(ID),
    string(Autor),
    isListaInteger(Fecha),
    string(Contenido),
    integer(Likes).

socialnetwork(Name, Fecha, [Name, Fecha, [], [], []]):-
    string(Name),
    isListaInteger(Fecha).

%Pertenencia

isListaInteger([]):- true, !.
isListaInteger([LI_X|LI_Y]):-
    integer(LI_X),
    isListaInteger(LI_Y).

isListaString([]):- true, !.
isListaString([LS_X| LS_Y]):-
    string(LS_X),
    isListaString(LS_Y).

isListaComp([]):- true, !.
isListaComp([LC_X|LC_Y]):-
    git_Universal(LC_X, 1, 1, [0,1], LC_XX),
    git_Universal(LC_X, 1, 2, [0,1], LC_XY),
    integer(LC_XX),
    isListaInteger(LC_XY),
    isListaComp(LC_Y).

isListaUser([]):- true, !.
isListaUser([LU_X|LU_Y]):-
    usuario(_,_,_,_,_,_,_,_,_,_,_,LU_X),
    isListaUser(LU_Y).

isListaPost([]):- true, !.
isListaPost([LP_X|LP_Y]):-
    publicacion(_,_,_,_,_,_,_,_,LP_X),
    isListaPost(LP_Y).

isListaComent([]):- true, !.
isListaComent([LC_X|LC_Y]):-
    comentario(_,_,_,_,_,LC_X),
    isListaComent(LC_Y).


isSocialNetwork([Name, Fecha, ListUser, ListPost, ListComent]):-
    string(Name),
    isListaInteger(Fecha),
    isListaUser(ListUser),
    isListaPost(ListPost),
    isListaComent(ListComent).


%selectores

%Selector universal
%git_Universal(Lista socialnetwork, donde empieza, donde terminara, una lista de recorrido para saber si es dentro de una lista o no, donde se guarda)
git_Universal([],_,_,_,_):- false, !.
git_Universal([X_SN|_],Inicio,Max,[X_Lista|[_|_]],GUARDADO):-
    Inicio = Max,
    X_Lista = 0,
    GUARDADO = X_SN, !.
git_Universal([X_SN|_],Inicio,Max,[X_Lista|[XY_Lista|_]],GUARDADO):-
    Inicio = Max,
    X_Lista = 1,
    git_Universal(X_SN, 1,XY_Lista,[0,1],GUARDADO).
git_Universal([_|Y_SN],Inicio,Max,[X_Lista|[XY_Lista|Y_Lista]],GUARDADO):-
    V is Inicio + 1,
    git_Universal(Y_SN,V,Max,[X_Lista,XY_Lista,Y_Lista],GUARDADO).

% Selector del mayor ID, es decir vee cual es el ID mayor que se puede usar
% git_MayorID(Lista (User,Public, coment), ID de donde empieza, Lugar de la lista a buscar, Salida del ID)
git_MayorID([],ID,_,IDSalida):-
    IDSalida is ID, !.
git_MayorID([_|Y_List],ID,Max,IDSalida):-
    Max = 0,
    V is ID + 1,
    git_MayorID(Y_List, V, Max, IDSalida).
git_MayorID([X_List|Y_List], ID, Max, IDSalida):-
    git_Universal([X_List| Y_List], 1, Max, [0,1], List_X),
    git_MayorID(List_X, ID, 0, IDSalida).


% Permite saber si existe un usuario en una lista de usuarios
existeUser([],_):- false, !.
existeUser([X_ListUser| _], Nombre):-
    git_Universal(X_ListUser,1,2,[0,1], NameOut),
    NameOut == Nombre,
    true, !.
existeUser([_| Y_ListUser], Nombre):-
    existeUser(Y_ListUser, Nombre).

% Permita cambiar la actividad de un usuario
setActividad([],_, _, Aux, LUOut):- LUOut = Aux, true, !.
setActividad([X_LU | Y_LU], Nombre, Contrasena, Aux, LUOut):-
    git_Universal(X_LU,1,2,[0,1], NameOut),
    not(NameOut == Nombre),
    append(Aux, [X_LU], NewList),
    setActividad(Y_LU, Nombre, Contrasena, NewList, LUOut).
setActividad([X_LU | Y_LU], Nombre, Contrasena, Aux, LUOut):-
    git_Universal(X_LU,1,2,[0,1], NameOut),
    NameOut == Nombre,
    git_Universal(X_LU,1,3,[0,1], PassOut),
    not(PassOut = Contrasena),
    append(Aux, [X_LU], NewList),
    setActividad(Y_LU, Nombre, Contrasena, NewList, LUOut).
setActividad([X_LU | Y_LU], Nombre, Contrasena, Aux, LUOut):-
    git_Universal(X_LU,1,2,[0,1], Uout2),
    Uout2 == Nombre,
    git_Universal(X_LU,1,3,[0,1], Uout3),
    Uout3 == Contrasena,
    git_Universal(X_LU,1,4,[0,1], Uout4),
    Uout4 = 0,
    NewActiviti is 1,
    git_Universal(X_LU,1,1,[0,1], Uout1),
    git_Universal(X_LU,1,5,[0,1], Uout5),
    git_Universal(X_LU,1,6,[0,1], Uout6),
    git_Universal(X_LU,1,7,[0,1], Uout7),
    git_Universal(X_LU,1,8,[0,1], Uout8),
    git_Universal(X_LU,1,9,[0,1], Uout9),
    git_Universal(X_LU,1,10,[0,1], Uout10),
    git_Universal(X_LU,1,11,[0,1], Uout11),
    usuario(Uout1,Uout2,Uout3,NewActiviti,Uout5,Uout6,Uout7,Uout8,Uout9,Uout10, Uout11, NewUser),
    append(Aux, [NewUser], NewList),
    setActividad(Y_LU, Nombre, Contrasena, NewList, LUOut).
setActividad([X_LU | Y_LU], Nombre, Contrasena, Aux, LUOut):-
    git_Universal(X_LU,1,2,[0,1], Uout2),
    Uout2 == Nombre,
    git_Universal(X_LU,1,3,[0,1], Uout3),
    Uout3 == Contrasena,
    git_Universal(X_LU,1,4,[0,1], Uout4),
    Uout4 == 1,
    NewActiviti is 0,
    git_Universal(X_LU,1,1,[0,1], Uout1),
    git_Universal(X_LU,1,5,[0,1], Uout5),
    git_Universal(X_LU,1,6,[0,1], Uout6),
    git_Universal(X_LU,1,7,[0,1], Uout7),
    git_Universal(X_LU,1,8,[0,1], Uout8),
    git_Universal(X_LU,1,9,[0,1], Uout9),
    git_Universal(X_LU,1,10,[0,1], Uout10),
    git_Universal(X_LU,1,11,[0,1], Uout11),
    usuario(Uout1,Uout2,Uout3,NewActiviti,Uout5,Uout6,Uout7,Uout8,Uout9,Uout10, Uout11, NewUser),
    append(Aux, [NewUser], NewList),
    setActividad(Y_LU, Nombre, Contrasena, NewList, LUOut).

%Comprueba si existe un usuario activado y lo retorna
cuentaActivada([], _):- false, !.
cuentaActivada([LU_X | _], Salida):-
    git_Universal(LU_X,1,4,[0,1], Actividad),
    Actividad == 1,
    Salida = LU_X,
    true, !.
cuentaActivada([_ | LU_Y], Salida):-
    cuentaActivada(LU_Y, Salida).

%Permite buscar una cuenta segun nombre
buscarCuenta([],_,_):- false, !.
buscarCuenta([LU_X | _], Nombre, Salida):-
    git_Universal(LU_X,1,2,[0,1], NombreOut),
    NombreOut == Nombre,
    Salida = LU_X,
    true, !.
buscarCuenta([_ | LU_Y],Nombre, Salida):-
    buscarCuenta(LU_Y,Nombre, Salida).

%Permite cambiar un usuario por otro
cambiarUsuario([],_,Aux,Salida):- Salida = Aux, true, !.
cambiarUsuario([LU_X | LU_Y], User, Aux, Salida):-
    git_Universal(User,1,2,[0,1], OName1),
    git_Universal(LU_X,1,2,[0,1], OName2),
    OName1 == OName2,
    append(Aux, [User], NewLU),
    cambiarUsuario(LU_Y, User, NewLU, Salida).
cambiarUsuario([LU_X | LU_Y], User, Aux, Salida):-
    append(Aux, [LU_X], NewLU),
    cambiarUsuario(LU_Y, User, NewLU, Salida).

%Agrega una pregunta a la lista de preguntas
agregarPregunta(SN, Fecha, Texto, Autor, POut):-
    git_Universal(SN, 1, 1, [0,1], SNOut1),
    git_Universal(SN, 1, 2, [0,1], SNOut2),
    git_Universal(SN, 1, 3, [0,1], SNOut3),
    git_Universal(SN, 1, 4, [0,1], ListPost),
    git_Universal(SN, 1, 5, [0,1], SNOut5),
    git_MayorID(ListPost, 1,0,ID),
    publicacion(ID,Autor, Fecha, "text", Texto, [], 0, 0, Publicacion),
    append(ListPost, [Publicacion], NewListPost),
    POut = [[SNOut1,SNOut2,SNOut3,NewListPost,SNOut5],ID],
    true, !.
agregarUsuarioPregunta(SN, Usuario, ID, Salida):-
    git_Universal(SN, 1, 1, [0,1], SNOut1),
    git_Universal(SN, 1, 2, [0,1], SNOut2),
    git_Universal(SN, 1, 3, [0,1], ListUsers),
    git_Universal(SN, 1, 4, [0,1], SNOut4),
    git_Universal(SN, 1, 5, [0,1], SNOut5),
    git_Universal(Usuario,1,1,[0,1], Uout1),
    git_Universal(Usuario,1,2,[0,1], Uout2),
    git_Universal(Usuario,1,3,[0,1], Uout3),
    git_Universal(Usuario,1,4,[0,1], Uout4),
    git_Universal(Usuario,1,5,[0,1], ListPostUser),
    git_Universal(Usuario,1,6,[0,1], Uout6),
    git_Universal(Usuario,1,7,[0,1], Uout7),
    git_Universal(Usuario,1,8,[0,1], Uout8),
    git_Universal(Usuario,1,9,[0,1], Uout9),
    git_Universal(Usuario,1,10,[0,1], Uout10),
    git_Universal(Usuario,1,11,[0,1], Uout11),
    append(ListPostUser, [ID], NewListPostUser),
    usuario(Uout1,Uout2,Uout3,Uout4,NewListPostUser,Uout6,Uout7,Uout8,Uout9, Uout10, Uout11, NewUser),
    cambiarUsuario(ListUsers, NewUser, [], NewListaUser),
    Salida = [SNOut1,SNOut2,NewListaUser,SNOut4,SNOut5],
    true, !.


agregarUsuarioCompartida(SN,Usuario, ID, Fecha, Salida):-
    git_Universal(SN, 1, 1, [0,1], SNOut1),
    git_Universal(SN, 1, 2, [0,1], SNOut2),
    git_Universal(SN, 1, 3, [0,1], ListUsers),
    git_Universal(SN, 1, 4, [0,1], SNOut4),
    git_Universal(SN, 1, 5, [0,1], SNOut5),
    git_Universal(Usuario,1,1,[0,1], Uout1),
    git_Universal(Usuario,1,2,[0,1], Uout2),
    git_Universal(Usuario,1,3,[0,1], Uout3),
    git_Universal(Usuario,1,4,[0,1], Uout4),
    git_Universal(Usuario,1,5,[0,1], Uout5),
    git_Universal(Usuario,1,6,[0,1], ListPostComp),
    git_Universal(Usuario,1,7,[0,1], Uout7),
    git_Universal(Usuario,1,8,[0,1], Uout8),
    git_Universal(Usuario,1,9,[0,1], Uout9),
    git_Universal(Usuario,1,10,[0,1], Uout10),
    git_Universal(Usuario,1,11,[0,1], Uout11),
    append(ListPostComp, [[ID, Fecha]], NewListPostCompUser),
    usuario(Uout1,Uout2,Uout3,Uout4,Uout5,NewListPostCompUser,Uout7,Uout8,Uout9, Uout10, Uout11, NewUser),
    cambiarUsuario(ListUsers, NewUser, [], NewListaUser),
    Salida = [SNOut1,SNOut2,NewListaUser,SNOut4,SNOut5],
    true, !.

%Register
socialNetworkRegister(SN, Fecha, Nombre, Contrasena, _):-
    not(isSocialNetwork(SN)),
    not(isListaInteger(Fecha)),
    not(string(Nombre)),
    not(string(Contrasena)),
    false, !.
socialNetworkRegister(SN, Fecha, Nombre, Contrasena, OSN):-
    git_Universal(SN, 1, 3, [0,1], Lout3),
    not(existeUser(Lout3, Nombre)),
    git_MayorID(SN,1,3, IDOut),
    usuario(IDOut, Nombre, Contrasena, 0, [], [], Fecha, 0, 0, [], [], UOut),
    git_Universal(SN, 1, 1, [0,1], Lout1),
    git_Universal(SN, 1, 2, [0,1], Lout2),
    append(Lout3, [UOut], NewListUser),
    git_Universal(SN, 1, 4, [0,1], Lout4),
    git_Universal(SN, 1, 5, [0,1], Lout5),
    OSN = [Lout1,Lout2, NewListUser, Lout4, Lout5],
    true, !.
socialNetworkRegister(SN,_,_,_,OSN):-
    OSN = SN,
    true, !.

%Login
socialNetworkLogin(SN, Nombre, Contrasena, _):-
    not(isSocialNetwork(SN)),
    not(string(Nombre)),
    not(string(Contrasena)),
    false, !.
socialNetworkLogin(SN, Nombre, Contrasena, OSN):-
    git_Universal(SN, 1, 3, [0,1], ListUsers),
    existeUser(ListUsers, Nombre),
    setActividad(ListUsers, Nombre, Contrasena, [],NewListUser),
    git_Universal(SN, 1, 1, [0,1], Lout1),
    git_Universal(SN, 1, 2, [0,1], Lout2),
    git_Universal(SN, 1, 4, [0,1], Lout4),
    git_Universal(SN, 1, 5, [0,1], Lout5),
    OSN = [Lout1,Lout2, NewListUser, Lout4, Lout5],
    true, !.
socialNetworkLogin(SN,_,_,OSN):-
    OSN = SN,
    true, !.

%Post
socialNetworkPost_otroUser(SN, _, [], SNO):- SNO = SN, true, !.
socialNetworkPost_otroUser(SN, ID, [ListaUsuario_X | ListaUsuario_Y], SNO):-
    git_Universal(SN, 1, 3, [0,1], ListUsers),
    buscarCuenta(ListUsers, ListaUsuario_X, Usuario),
    agregarUsuarioPregunta(SN, Usuario, ID, NewSN),
    socialNetworkPost_otroUser(NewSN, ID, ListaUsuario_Y, SNO).
    socialNetworkPost_otroUser(_, _, _, _):- false, !.

socialNetworkPost(SN, Fecha, Texto, ListUsers, _):-
    not(isSocialNetwork(SN)),
    not(isListaInteger(Fecha)),
    not(string(Texto)),
    not(isListaInteger(ListUsers)),
    false, !.
socialNetworkPost(SN, Fecha, Texto, [], SNO):-
    git_Universal(SN, 1, 3, [0,1], ListUsers),
    cuentaActivada(ListUsers, Usuario),
    git_Universal(Usuario, 1, 2, [0,1], Autor),
    agregarPregunta(SN, Fecha, Texto, Autor, NewSN),
    git_Universal(NewSN, 1, 1, [0,1], SN_part2),
    git_Universal(NewSN, 1, 2, [0,1], ID),
    agregarUsuarioPregunta(SN_part2, Usuario, ID, SNSalida),
    SNO = SNSalida,
    true, !.
socialNetworkPost(SN, Fecha, Texto, ListaUsuario, SNO):-
    git_Universal(SN, 1, 3, [0,1], ListUsers),
    cuentaActivada(ListUsers, UsuarioAutor),
    git_Universal(UsuarioAutor, 1, 2, [0,1], Autor),
    agregarPregunta(SN, Fecha, Texto, Autor, NewSN),
    git_Universal(NewSN, 1, 1, [0,1], SN_parte2),
    git_Universal(NewSN, 1, 2, [0,1], ID),
    socialNetworkPost_otroUser(SN_parte2, ID, ListaUsuario, SNO),
    true, !.
socialNetworkPost(_, _, _, _, _):- false, !.


%Follow


socialNetworkFollow(SN, User, _):-
    not(isSocialNetwork(SN)),
    not(string(User)),
    false, !.
socialNetworkFollow(SN, User, SNOut):-
    git_Universal(SN, 1, 1, [0,1], NombreSN),
    git_Universal(SN, 1, 2, [0,1], FechaSN),
    git_Universal(SN, 1, 3, [0,1], ListUsersSN),
    git_Universal(SN, 1, 4, [0,1], ListPostSN),
    git_Universal(SN, 1, 5, [0,1], ListComentSN),
    cuentaActivada(ListUsersSN, UsuarioActivoado),
    buscarCuenta(ListUsersSN, User, UsuarioSeguir),
    git_Universal(UsuarioActivoado,1,1,[0,1], SUA1),
    git_Universal(UsuarioActivoado,1,2,[0,1], NameUserActi),
    git_Universal(UsuarioActivoado,1,3,[0,1], SUA3),
    git_Universal(UsuarioActivoado,1,4,[0,1], SUA4),
    git_Universal(UsuarioActivoado,1,5,[0,1], SUA5),
    git_Universal(UsuarioActivoado,1,6,[0,1], SUA6),
    git_Universal(UsuarioActivoado,1,7,[0,1], SUA7),
    git_Universal(UsuarioActivoado,1,8,[0,1], CantUserSeg_UA),
    CantUserSeg_UA_Total is CantUserSeg_UA + 1,
    git_Universal(UsuarioActivoado,1,9,[0,1], SUA9),
    git_Universal(UsuarioActivoado,1,10,[0,1], ListUserSeg_UA),
    append(ListUserSeg_UA, [User], NewListUserSeg_UA),
    git_Universal(UsuarioActivoado,1,11,[0,1], SUA11),
    usuario(SUA1,NameUserActi,SUA3,SUA4,SUA5,SUA6,SUA7,CantUserSeg_UA_Total,SUA9,NewListUserSeg_UA,SUA11, NewUserAct),
    git_Universal(UsuarioSeguir,1,1,[0,1], SU1),
    git_Universal(UsuarioSeguir,1,2,[0,1], SU2),
    git_Universal(UsuarioSeguir,1,3,[0,1], SU3),
    git_Universal(UsuarioSeguir,1,4,[0,1], SU4),
    git_Universal(UsuarioSeguir,1,5,[0,1], SU5),
    git_Universal(UsuarioSeguir,1,6,[0,1], SU6),
    git_Universal(UsuarioSeguir,1,7,[0,1], SU7),
    git_Universal(UsuarioSeguir,1,8,[0,1], SU8),
    git_Universal(UsuarioSeguir,1,9,[0,1], CantUserSeg_U),
    CantUserSeg_U_Total is CantUserSeg_U + 1,
    git_Universal(UsuarioSeguir,1,10,[0,1], SU10),
    git_Universal(UsuarioSeguir,1,11,[0,1], ListUserSeg_U),
    append(ListUserSeg_U, [NameUserActi], NewListUserSeg_U),
    usuario(SU1,SU2,SU3,SU4,SU5,SU6,SU7,SU8,CantUserSeg_U_Total,SU10,NewListUserSeg_U, NewUser),
    cambiarUsuario(ListUsersSN, NewUserAct, [], NewListUser1),
    cambiarUsuario(NewListUser1, NewUser, [],NewListUser2),
    SNOut = [NombreSN, FechaSN, NewListUser2, ListPostSN, ListComentSN],
    true, !.


% Share

socialNetworkShare_otroUser(SN,_,_,[], SOut):- SOut = SN, true, !.
socialNetworkShare_otroUser(SN,Fecha,IDPost, [ListUser_X|ListUser_Y], SOut):-
    git_Universal(SN, 1, 3, [0,1], ListUsers),
    buscarCuenta(ListUsers, ListUser_X, Usuario),
    agregarUsuarioCompartida(SN, Usuario, IDPost, Fecha, NewSN),
    socialNetworkShare_otroUser(NewSN,Fecha,IDPost, ListUser_Y, SOut).
socialNetworkShare_otroUser(_, _, _, _, _):- false, !.

socialNetworkShare(SN, Fecha, IDPost, ListUserName, _):-
    not(isSocialNetwork(SN)),
    not(isListaInteger(Fecha)),
    not(integer(IDPost)),
    not(isListaString(ListUserName)),
    false, !.

socialNetworkShare(SN,Fecha,IDPost,[], SOut):-
    git_Universal(SN, 1, 3, [0,1], ListUsers),
    cuentaActivada(ListUsers, Usuario),
    agregarUsuarioCompartida(SN, Usuario, IDPost, Fecha, SNSalida),
    SOut = SNSalida,
    true, !.
socialNetworkShare(SN,Fecha,IDPost,ListUser, SOut):-
    socialNetworkShare_otroUser(SN,Fecha,IDPost,ListUser, SOut),
    true, !.
