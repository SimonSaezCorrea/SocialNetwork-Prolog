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
    Lista de Comentario     -> Lista, Corresponde a los comentarios que contiene

socialnetwork(Name, Fecha de creacion, Lista de Usuarios, Lista de Publicaciones, Lista de Comentarios):
    Name                    -> String, Corresponde al nombre del socialnetwork
    Fecha de creacion       -> Lista, Corresponde a la fecha de creacion
    Lista de Usuarios       -> Lista, Corresponde a la lista de usuarios en el socialnetwork
    Lista de Publicaciones  -> Lista, Corresponde a la lista de Publicaciones en el socialnetwork
    Lista de Comentarios    -> Lista, Corresponde a la lista de Comentarios en el socialnetwork
*/

%Metas
/*
Principales ->  fecha
                socialnetwork
                socialNetworkRegister
                socialNetworkLogin
                socialNetworkPost
                socialNetworkFollow
                socialNetworkShare
                socialNetworkToString
                socialNetworkComment
                socialNetworkLike

Secundarias ->  usuario, publicacion, comentario
                isListaInteger, isListaString, isListaComp, isListaUser, isListaPost, isListaComent, isSocialNetwork
                existeComentario, existePost, existeUser, existeActivo
                git_Universal, git_MayorID, getPregunta, getComentario
                setActividad, setDesconectado
                cuentaActivada, buscarCuenta, cambiar
                agregarPregunta, agregarUsuarioPregunta, agregarUsuarioCompartida
                agregarComentario, agregarComentarioPregunta, agregarComentarioComentario
                socialNetworkPost_otroUser, socialNetworkShare_otroUser
                comentUserToString, pregUserToString, pregCompUserToString, listStringToString, userToString, listUserToString
*/



% --------------------------------Constructores---------------------------

%Permite crear un usuario
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

%Permite crear una publicacion
publicacion(ID,Autor, Fecha, Tipo, Contenido, ListComentarios, Likes, CantidadCompartida, [ID,Autor, Fecha, Tipo, Contenido, ListComentarios, Likes, CantidadCompartida]):-
    integer(ID),
    string(Autor),
    isListaInteger(Fecha),
    string(Tipo),
    string(Contenido),
    isListaInteger(ListComentarios),
    integer(Likes),
    integer(CantidadCompartida).

%Permite crear un comentario
comentario(ID, Autor, Fecha, Contenido, Likes, ListComentario, [ID, Autor, Fecha, Contenido, Likes, ListComentario]):-
    integer(ID),
    string(Autor),
    isListaInteger(Fecha),
    string(Contenido),
    integer(Likes),
    isListaInteger(ListComentario).

%Permite crear una social network
socialnetwork(Name, Fecha, [Name, Fecha, [], [], []]):-
    string(Name),
    isListaInteger(Fecha).



% ------------------------------------Pertenencia-------------------------------

%Permite saber si una lista es de enteros
isListaInteger([]):- true, !.
isListaInteger([LI_X|LI_Y]):-
    integer(LI_X),
    isListaInteger(LI_Y).

%Permite saber si una lista es de strings
isListaString([]):- true, !.
isListaString([LS_X| LS_Y]):-
    string(LS_X),
    isListaString(LS_Y).

%Permite saber si una lista de compartidos corresponde a ello
isListaComp([]):- true, !.
isListaComp([LC_X|LC_Y]):-
    git_Universal(LC_X, 1, 1, LC_XX),
    git_Universal(LC_X, 1, 2, LC_XY),
    integer(LC_XX),
    isListaInteger(LC_XY),
    isListaComp(LC_Y).

%Permite saber si es una lista de usuarios
isListaUser([]):- true, !.
isListaUser([LU_X|LU_Y]):-
    usuario(_,_,_,_,_,_,_,_,_,_,_,LU_X),
    isListaUser(LU_Y).

%Permite saber si es una lista de publicaciones
isListaPost([]):- true, !.
isListaPost([LP_X|LP_Y]):-
    publicacion(_,_,_,_,_,_,_,_,LP_X),
    isListaPost(LP_Y).

%Permite saber si es una lista de comentarios
isListaComent([]):- true, !.
isListaComent([LC_X|LC_Y]):-
    comentario(_,_,_,_,_,_,LC_X),
    isListaComent(LC_Y).

%Permite saber si es una social network
isSocialNetwork([Name, Fecha, ListUser, ListPost, ListComent]):-
    string(Name),
    isListaInteger(Fecha),
    isListaUser(ListUser),
    isListaPost(ListPost),
    isListaComent(ListComent).

%Permite saber si existe un comentario con tal ID
existeComentario([],_):- false, !.
existeComentario([LP_X|_], IDC):-
    LP_X = IDC,
    true, !.
existeComentario([_|LP_Y], IDC):-
    existeComentario(LP_Y, IDC).

%Permite saber si existe un post con tal ID
existePost([],_):- false, !.
existePost([[ID|_]|_], IDP):-
    ID = IDP,
    true, !.
existePost([_|LP_Y], IDC):-
    existePost(LP_Y, IDC).

%Permite saber si existe un usuario
existeUser([],_):- false, !.
existeUser([X_ListUser| _], Nombre):-
    git_Universal(X_ListUser,1,2, NameOut),
    NameOut == Nombre,
    true, !.
existeUser([_| Y_ListUser], Nombre):-
    existeUser(Y_ListUser, Nombre).

%Permite saber si existe un usuario activo
existeActivo([]):-false, !.
existeActivo([SN_X|_]):-
    git_Universal(SN_X,1,4,Actividad),
    Actividad == 1.
existeActivo([_|SN_Y]):-
    existeActivo(SN_Y).


%----------------------------------selectores------------------------

%Selector universal


%Permite rescatar algun elemento de lo que sea (Universal)
%DOM -> Lista que se quiere recorrer, donde empieza (1 idealmente),
%       donde termina, Salida
git_Universal([],_,_,_):- false, !.
git_Universal([X_SN|_],Inicio,Max,GUARDADO):-
    Inicio = Max,
    GUARDADO = X_SN,
    true, !.
git_Universal([_|Y_SN],Inicio,Max,GUARDADO):-
    V is Inicio + 1,
    git_Universal(Y_SN,V,Max,GUARDADO).

%Selector del mayor ID, es decir vee cual es el ID mayor que se puede usar
git_MayorID([],ID,_,IDSalida):-
    IDSalida is ID, !.
git_MayorID([_|Y_List],ID,Max,IDSalida):-
    Max = 0,
    V is ID + 1,
    git_MayorID(Y_List, V, Max, IDSalida).
git_MayorID([X_List|Y_List], ID, Max, IDSalida):-
    git_Universal([X_List| Y_List], 1, Max, List_X),
    git_MayorID(List_X, ID, 0, IDSalida).

%Permite recoger una pregunta segun ID
getPregunta([LP_X|_], ID, Salida):-
    git_Universal(LP_X,1,1,IDLP),
    IDLP == ID,
    Salida = LP_X.
getPregunta([_|LP_Y],ID,Salida):-
    getPregunta(LP_Y, ID, Salida).

%Permite recoger un comentario segun ID
getComentario([LC_X|_], ID, Salida):-
    git_Universal(LC_X,1,1,IDLC),
    IDLC == ID,
    Salida = LC_X.
getComentario([_|LC_Y],ID,Salida):-
    getComentario(LC_Y, ID, Salida).




% Permita cambiar la actividad de un usuario de 0 -> 1
setActividad([],_, _, Aux, LUOut):- LUOut = Aux, true, !.
setActividad([X_LU | Y_LU], Nombre, Contrasena, Aux, LUOut):-
    git_Universal(X_LU,1,2, NameOut),
    not(NameOut == Nombre),
    append(Aux, [X_LU], NewList),
    setActividad(Y_LU, Nombre, Contrasena, NewList, LUOut).
setActividad([X_LU | Y_LU], Nombre, Contrasena, Aux, LUOut):-
    git_Universal(X_LU,1,2, NameOut),
    NameOut == Nombre,
    git_Universal(X_LU,1,3, PassOut),
    not(PassOut = Contrasena),
    append(Aux, [X_LU], NewList),
    setActividad(Y_LU, Nombre, Contrasena, NewList, LUOut).
setActividad([X_LU | Y_LU], Nombre, Contrasena, Aux, LUOut):-
    git_Universal(X_LU,1,2, Uout2),
    Uout2 == Nombre,
    git_Universal(X_LU,1,3, Uout3),
    Uout3 == Contrasena,
    git_Universal(X_LU,1,4, Uout4),
    Uout4 = 0,
    NewActiviti is 1,
    git_Universal(X_LU,1,1, Uout1),
    git_Universal(X_LU,1,5, Uout5),
    git_Universal(X_LU,1,6, Uout6),
    git_Universal(X_LU,1,7, Uout7),
    git_Universal(X_LU,1,8, Uout8),
    git_Universal(X_LU,1,9, Uout9),
    git_Universal(X_LU,1,10, Uout10),
    git_Universal(X_LU,1,11, Uout11),
    usuario(Uout1,Uout2,Uout3,NewActiviti,Uout5,Uout6,Uout7,Uout8,Uout9,Uout10, Uout11, NewUser),
    append(Aux, [NewUser], NewList),
    setActividad(Y_LU, Nombre, Contrasena, NewList, LUOut).
setActividad([X_LU|Y_LU],Nombre,Contrasena,Aux,LUOut):-
     append(Aux, [X_LU], NewList),
     setActividad(Y_LU, Nombre, Contrasena, NewList, LUOut).

%Permite cambiar la actividad de un usuario de 1 -> 0
setDesconectado([],Aux,LUOut):- LUOut = Aux, true, !.
setDesconectado([X_LU | Y_LU], Aux, LUOut):-
    git_Universal(X_LU,1,4, Uout4),
    Uout4 == 1,
    NewActiviti = 0,
    git_Universal(X_LU,1,1, Uout1),
    git_Universal(X_LU,1,2, Uout2),
    git_Universal(X_LU,1,3, Uout3),
    git_Universal(X_LU,1,5, Uout5),
    git_Universal(X_LU,1,6, Uout6),
    git_Universal(X_LU,1,7, Uout7),
    git_Universal(X_LU,1,8, Uout8),
    git_Universal(X_LU,1,9, Uout9),
    git_Universal(X_LU,1,10, Uout10),
    git_Universal(X_LU,1,11, Uout11),
    usuario(Uout1,Uout2,Uout3,NewActiviti,Uout5,Uout6,Uout7,Uout8,Uout9,Uout10, Uout11, NewUser),
    append(Aux, [NewUser], NewList),
    setDesconectado(Y_LU, NewList, LUOut).
setDesconectado([X_LU|Y_LU], Aux, LUOut):-
    append(Aux, [X_LU], NewList),
    setDesconectado(Y_LU, NewList, LUOut).


%-----------------------Otras funciones------------------------------

%Permite retorna una cuenta activa
cuentaActivada([], _):- false, !.
cuentaActivada([LU_X | _], Salida):-
    git_Universal(LU_X,1,4, Actividad),
    Actividad == 1,
    Salida = LU_X,
    true, !.
cuentaActivada([_ | LU_Y], Salida):-
    cuentaActivada(LU_Y, Salida).

%Permite buscar una cuenta segun nombre
buscarCuenta([],_,_):- false, !.
buscarCuenta([LU_X | _], Nombre, Salida):-
    git_Universal(LU_X,1,2, NombreOut),
    NombreOut == Nombre,
    Salida = LU_X,
    true, !.
buscarCuenta([_ | LU_Y],Nombre, Salida):-
    buscarCuenta(LU_Y,Nombre, Salida).

%Permite cambiar algo por otra cosa segun su ID
cambiar([],_,Aux,Salida):- Salida = Aux, true, !.
cambiar([LU_X | LU_Y], User, Aux, Salida):-
    git_Universal(User,1,1, ID1),
    git_Universal(LU_X,1,1, ID2),
    ID1 == ID2,
    append(Aux, [User], NewLU),
    cambiar(LU_Y, User, NewLU, Salida).
cambiar([LU_X | LU_Y], User, Aux, Salida):-
    append(Aux, [LU_X], NewLU),
    cambiar(LU_Y, User, NewLU, Salida).

%Agrega una pregunta a la lista de post
agregarPregunta(SN, Fecha, Texto, Autor, POut):-
    git_Universal(SN, 1, 1, SNOut1),
    git_Universal(SN, 1, 2, SNOut2),
    git_Universal(SN, 1, 3, SNOut3),
    git_Universal(SN, 1, 4, ListPost),
    git_Universal(SN, 1, 5, SNOut5),
    git_MayorID(ListPost, 1,0,ID),
    publicacion(ID,Autor, Fecha, "text", Texto, [], 0, 0, Publicacion),
    append(ListPost, [Publicacion], NewListPost),
    POut = [[SNOut1,SNOut2,SNOut3,NewListPost,SNOut5],ID],
    true, !.

%Permite agregar a un usuario un post (ID de la pregunta)
agregarUsuarioPregunta(SN, Usuario, ID, Salida):-
    git_Universal(SN,1,1,SNOut1),
    git_Universal(SN,1,2,SNOut2),
    git_Universal(SN,1,3,ListUsers),
    git_Universal(SN,1,4,SNOut4),
    git_Universal(SN,1,5,SNOut5),
    git_Universal(Usuario,1,1,Uout1),
    git_Universal(Usuario,1,2,Uout2),
    git_Universal(Usuario,1,3,Uout3),
    git_Universal(Usuario,1,4,Uout4),
    git_Universal(Usuario,1,5,ListPostUser),
    git_Universal(Usuario,1,6,Uout6),
    git_Universal(Usuario,1,7,Uout7),
    git_Universal(Usuario,1,8,Uout8),
    git_Universal(Usuario,1,9,Uout9),
    git_Universal(Usuario,1,10,Uout10),
    git_Universal(Usuario,1,11,Uout11),
    append(ListPostUser, [ID], NewListPostUser),
    usuario(Uout1,Uout2,Uout3,Uout4,NewListPostUser,Uout6,Uout7,Uout8,Uout9, Uout10, Uout11, NewUser),
    cambiar(ListUsers, NewUser, [], NewListaUser),
    Salida = [SNOut1,SNOut2,NewListaUser,SNOut4,SNOut5],
    true, !.

%Permite agregar a un usuario un post compartido(ID)
agregarUsuarioCompartida(SN,Usuario, ID, Fecha, Salida):-
    git_Universal(SN,1,1,SNOut1),
    git_Universal(SN,1,2,SNOut2),
    git_Universal(SN,1,3,ListUsers),
    git_Universal(SN,1,4,ListPost),
    git_Universal(SN,1,5,SNOut5),
    existePost(ListPost, ID),
    git_Universal(Usuario,1,1,Uout1),
    git_Universal(Usuario,1,2,Uout2),
    git_Universal(Usuario,1,3,Uout3),
    git_Universal(Usuario,1,4,Uout4),
    git_Universal(Usuario,1,5,Uout5),
    git_Universal(Usuario,1,6,ListPostComp),
    git_Universal(Usuario,1,7,Uout7),
    git_Universal(Usuario,1,8,Uout8),
    git_Universal(Usuario,1,9,Uout9),
    git_Universal(Usuario,1,10,Uout10),
    git_Universal(Usuario,1,11,Uout11),
    append(ListPostComp, [[ID, Fecha]], NewListPostCompUser),
    usuario(Uout1,Uout2,Uout3,Uout4,Uout5,NewListPostCompUser,Uout7,Uout8,Uout9, Uout10, Uout11, NewUser),
    cambiar(ListUsers, NewUser, [], NewListaUser),
    Salida = [SNOut1,SNOut2,NewListaUser,ListPost,SNOut5],
    true, !.

%Permite agregar un comentario a la lista de comentarios
agregarComentario(SN, Fecha, Texto, Autor,COut):-
    git_Universal(SN,1,1,SNOut1),
    git_Universal(SN,1,2,SNOut2),
    git_Universal(SN,1,3,SNOut3),
    git_Universal(SN,1,4,SNOut4),
    git_Universal(SN,1,5,ListComent),
    git_MayorID(ListComent, 1,0,ID),
    comentario(ID,Autor, Fecha, Texto, 0, [], Comentario),
    append(ListComent, [Comentario], NewListComment),
    COut = [[SNOut1,SNOut2,SNOut3,SNOut4,NewListComment], ID],
    true, !.

%Permite agregar a un post un comentario (ID)
agregarComentarioPregunta(SN,IDP,IDC,COut):-
    git_Universal(SN,1,1,SNOut1),
    git_Universal(SN,1,2,SNOut2),
    git_Universal(SN,1,3,SNOut3),
    git_Universal(SN,1,4,ListPost),
    git_Universal(SN,1,5,SNOut5),
    getPregunta(ListPost, IDP, Post),
    git_Universal(Post,1,1,P1),
    git_Universal(Post,1,2,P2),
    git_Universal(Post,1,3,P3),
    git_Universal(Post,1,4,P4),
    git_Universal(Post,1,5,P5),
    git_Universal(Post,1,6,P6),
    git_Universal(Post,1,7,P7),
    git_Universal(Post,1,8,P8),
    append(P6, [IDC], NewP6),
    publicacion(P1,P2,P3,P4,P5,NewP6,P7,P8, NewPost),
    cambiar(ListPost, NewPost, [], NewListPost),
    COut = [SNOut1, SNOut2, SNOut3, NewListPost, SNOut5],
    true, !.

%Permite agregar un comentario a un comentario
agregarComentarioComentario(SN,IDP,IDC,ID,COut):-
    git_Universal(SN,1,1,SNOut1),
    git_Universal(SN,1,2,SNOut2),
    git_Universal(SN,1,3,SNOut3),
    git_Universal(SN,1,4,ListPost),
    git_Universal(SN,1,5,ListComent),
    getPregunta(ListPost, IDP, Post),
    git_Universal(Post,1,6,P6),
    existeComentario(P6, IDC),
    getComentario(ListComent, IDC, Comentario),
    git_Universal(Comentario,1,1,C1),
    git_Universal(Comentario,1,2,C2),
    git_Universal(Comentario,1,3,C3),
    git_Universal(Comentario,1,4,C4),
    git_Universal(Comentario,1,5,C5),
    git_Universal(Comentario,1,6,C6),
    append(C6, [ID], NewC6),
    comentario(C1,C2,C3,C4,C5,NewC6, NewComent),
    cambiar(ListComent, NewComent, [], NewListComent),
    COut = [SNOut1, SNOut2, SNOut3, ListPost, NewListComent],
    true, !.



%..........................Register..............................
socialNetworkRegister(SN, Fecha, Nombre, Contrasena, _):-
    not(isSocialNetwork(SN)),
    not(isListaInteger(Fecha)),
    not(string(Nombre)),
    not(string(Contrasena)),
    false, !.
socialNetworkRegister(SN, Fecha, Nombre, Contrasena, OSN):-
    git_Universal(SN,1,3,Lout3),
    not(existeUser(Lout3, Nombre)),
    git_MayorID(SN,1,3, IDOut),
    usuario(IDOut, Nombre, Contrasena, 0, [], [], Fecha, 0, 0, [], [], UOut),
    git_Universal(SN,1,1,Lout1),
    git_Universal(SN,1,2,Lout2),
    append(Lout3, [UOut], NewListUser),
    git_Universal(SN,1,4,Lout4),
    git_Universal(SN,1,5,Lout5),
    OSN = [Lout1,Lout2, NewListUser, Lout4, Lout5],
    true, !.
socialNetworkRegister(SN,_,_,_,OSN):-
    OSN = SN,
    true, !.

%...........................Login................................
socialNetworkLogin(SN, Nombre, Contrasena, _):-
    not(isSocialNetwork(SN)),
    not(string(Nombre)),
    not(string(Contrasena)),
    false, !.
socialNetworkLogin(SN, Nombre, Contrasena, OSN):-
    git_Universal(SN,1,3,ListUsers),
    existeUser(ListUsers, Nombre),
    not(existeActivo(ListUsers)),
    setActividad(ListUsers, Nombre, Contrasena, [],NewListUser),
    existeActivo(NewListUser),
    git_Universal(SN,1,1,Lout1),
    git_Universal(SN,1,2,Lout2),
    git_Universal(SN,1,4,Lout4),
    git_Universal(SN,1,5,Lout5),
    OSN = [Lout1,Lout2, NewListUser, Lout4, Lout5],
    true, !.
socialNetworkLogin(SN,_,_,OSN):-
    OSN = SN,
    false, !.

%..........................Post......................................
socialNetworkPost_otroUser(SN, _, [], SNO):- SNO = SN, true, !.
socialNetworkPost_otroUser(SN, ID, [ListaUsuario_X | ListaUsuario_Y], SNO):-
    git_Universal(SN,1,3,ListUsers),
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
    git_Universal(SN,1,3,ListUsers),
    cuentaActivada(ListUsers, Usuario),
    git_Universal(Usuario,1,2,Autor),
    agregarPregunta(SN, Fecha, Texto, Autor, NewSN),
    git_Universal(NewSN,1,1,SN_part2),
    git_Universal(NewSN,1,2,ID),
    agregarUsuarioPregunta(SN_part2, Usuario, ID, SNSalida),
    git_Universal(SNSalida,1,3,ListUsers2),
    setDesconectado(ListUsers2, [],NewListUser),
    git_Universal(SNSalida,1,1,Lout1),
    git_Universal(SNSalida,1,2,Lout2),
    git_Universal(SNSalida,1,4,Lout4),
    git_Universal(SNSalida,1,5,Lout5),
    SNO = [Lout1,Lout2, NewListUser, Lout4, Lout5],
    true, !.
socialNetworkPost(SN, Fecha, Texto, ListaUsuario, SNO):-
    git_Universal(SN,1,3,ListUsers),
    cuentaActivada(ListUsers, UsuarioAutor),
    git_Universal(UsuarioAutor,1,2,Autor),
    agregarPregunta(SN, Fecha, Texto, Autor, NewSN),
    git_Universal(NewSN,1,1,SN_parte2),
    git_Universal(NewSN,1,2,ID),
    socialNetworkPost_otroUser(SN_parte2, ID, ListaUsuario, OSN),
    git_Universal(OSN,1,3,ListUsers2),
    setDesconectado(ListUsers2, [],NewListUser),
    git_Universal(OSN,1,1,Lout1),
    git_Universal(OSN,1,2,Lout2),
    git_Universal(OSN,1,4,Lout4),
    git_Universal(OSN,1,5,Lout5),
    SNO = [Lout1,Lout2, NewListUser, Lout4, Lout5],
    true, !.
socialNetworkPost(_, _, _, _, _):- false, !.


%............................Follow..............................
socialNetworkFollow(SN, User, _):-
    not(isSocialNetwork(SN)),
    not(string(User)),
    false, !.
socialNetworkFollow(SN, User, SNOut):-
    git_Universal(SN,1,1,NombreSN),
    git_Universal(SN,1,2,FechaSN),
    git_Universal(SN,1,3,ListUsersSN),
    git_Universal(SN,1,4,ListPostSN),
    git_Universal(SN,1,5,ListComentSN),
    cuentaActivada(ListUsersSN, UsuarioActivoado),
    buscarCuenta(ListUsersSN, User, UsuarioSeguir),
    git_Universal(UsuarioActivoado,1,1,SUA1),
    git_Universal(UsuarioActivoado,1,2,NameUserActi),
    git_Universal(UsuarioActivoado,1,3,SUA3),
    git_Universal(UsuarioActivoado,1,4,SUA4),
    git_Universal(UsuarioActivoado,1,5,SUA5),
    git_Universal(UsuarioActivoado,1,6,SUA6),
    git_Universal(UsuarioActivoado,1,7,SUA7),
    git_Universal(UsuarioActivoado,1,8,CantUserSeg_UA),
    not(NameUserActi == User),
    CantUserSeg_UA_Total is CantUserSeg_UA + 1,
    git_Universal(UsuarioActivoado,1,9,SUA9),
    git_Universal(UsuarioActivoado,1,10,ListUserSeg_UA),
    append(ListUserSeg_UA, [User], NewListUserSeg_UA),
    git_Universal(UsuarioActivoado,1,11,SUA11),
    usuario(SUA1,NameUserActi,SUA3,SUA4,SUA5,SUA6,SUA7,CantUserSeg_UA_Total,SUA9,NewListUserSeg_UA,SUA11, NewUserAct),
    git_Universal(UsuarioSeguir,1,1,SU1),
    git_Universal(UsuarioSeguir,1,2,SU2),
    git_Universal(UsuarioSeguir,1,3,SU3),
    git_Universal(UsuarioSeguir,1,4,SU4),
    git_Universal(UsuarioSeguir,1,5,SU5),
    git_Universal(UsuarioSeguir,1,6,SU6),
    git_Universal(UsuarioSeguir,1,7,SU7),
    git_Universal(UsuarioSeguir,1,8,SU8),
    git_Universal(UsuarioSeguir,1,9,CantUserSeg_U),
    CantUserSeg_U_Total is CantUserSeg_U + 1,
    git_Universal(UsuarioSeguir,1,10,SU10),
    git_Universal(UsuarioSeguir,1,11,ListUserSeg_U),
    append(ListUserSeg_U, [NameUserActi], NewListUserSeg_U),
    usuario(SU1,SU2,SU3,SU4,SU5,SU6,SU7,SU8,CantUserSeg_U_Total,SU10,NewListUserSeg_U, NewUser),
    cambiar(ListUsersSN, NewUserAct, [], NewListUser1),
    cambiar(NewListUser1, NewUser, [],NewListUser2),
    SNOut_pre = [NombreSN, FechaSN, NewListUser2, ListPostSN, ListComentSN],
    git_Universal(SNOut_pre,1,3,ListUsers2),
    setDesconectado(ListUsers2, [],NewListUser),
    git_Universal(SNOut_pre,1,1,Lout1),
    git_Universal(SNOut_pre,1,2,Lout2),
    git_Universal(SNOut_pre,1,4,Lout4),
    git_Universal(SNOut_pre,1,5,Lout5),
    SNOut = [Lout1,Lout2, NewListUser, Lout4, Lout5],
    true, !.


% .................................Share..................................
socialNetworkShare_otroUser(SN,_,_,[], SOut):- SOut = SN, true, !.
socialNetworkShare_otroUser(SN,Fecha,IDPost, [ListUser_X|ListUser_Y], SOut):-
    git_Universal(SN,1,3,ListUsers),
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
    git_Universal(SN,1,3,ListUsers),
    cuentaActivada(ListUsers, Usuario),
    agregarUsuarioCompartida(SN, Usuario, IDPost, Fecha, SNSalida),
    SNOut_pre = SNSalida,
    git_Universal(SNOut_pre,1,3,ListUsers2),
    setDesconectado(ListUsers2, [],NewListUser),
    git_Universal(SNOut_pre,1,1,Lout1),
    git_Universal(SNOut_pre,1,2,Lout2),
    git_Universal(SNOut_pre,1,4,Lout4),
    git_Universal(SNOut_pre,1,5,Lout5),
    SOut = [Lout1,Lout2, NewListUser, Lout4, Lout5],

    true, !.
socialNetworkShare(SN,Fecha,IDPost,ListUser, SOut):-
    socialNetworkShare_otroUser(SN,Fecha,IDPost,ListUser, SNOut_pre),
    git_Universal(SNOut_pre,1,3,ListUsers2),
    setDesconectado(ListUsers2, [],NewListUser),
    git_Universal(SNOut_pre,1,1,Lout1),
    git_Universal(SNOut_pre,1,2,Lout2),
    git_Universal(SNOut_pre,1,4,Lout4),
    git_Universal(SNOut_pre,1,5,Lout5),
    SOut = [Lout1,Lout2, NewListUser, Lout4, Lout5],
    true, !.

%...........................String..............................
comentUserToString([],_,String,String).
comentUserToString([LID_X|LID_Y],LC, String, StringSalida):-
    getPregunta(LC,LID_X,Coment),
    git_Universal(Coment,1,1,ID),
    git_Universal(Coment,1,2,Autor),
    git_Universal(Coment,1,4,Contenido),
    git_Universal(Coment,1,5,Likes),
    concat(String, "        El id es ", String1),
    concat(String1, ID, String2),
    concat(String2, "\n", String3),
    concat(String3, "        El autor es ", String4),
    concat(String4, Autor, String5),
    concat(String5, "\n        ", String6),
    concat(String6, Contenido, String7),
    concat(String7, "\n", String8),
    concat(String8, "        Tiene ", String9),
    concat(String9, Likes, String10),
    concat(String10, " likes\n---------------------------------------\n", String11),
    comentUserToString(LID_Y, LC, String11, StringSalida).


pregUserToString([],_,_,String, String).
pregUserToString([LID_X|LID_Y], LP, LC, String, StringSalida):-
    getPregunta(LP,LID_X,Preg),
    git_Universal(Preg,1,1,ID),
    git_Universal(Preg,1,2,Autor),
    git_Universal(Preg,1,5,Contenido),
    git_Universal(Preg,1,6,ListaComentario),
    git_Universal(Preg,1,7,Likes),
    concat(String, "    El id es ", String1),
    concat(String1, ID, String2),
    concat(String2, "\n", String3),
    concat(String3, "    El autor es ", String4),
    concat(String4, Autor, String5),
    concat(String5, "\n    ", String6),
    concat(String6, Contenido, String7),
    concat(String7, "\n", String8),
    concat(String8, "    Comentarios: \n", String9),
    comentUserToString(ListaComentario, LC, String9, String10),
    concat(String10, "    Tiene ", String11),
    concat(String11, Likes, String12),
    concat(String12, " likes\n---------------------------------------\n", String13),
    pregUserToString(LID_Y, LP, LC, String13, StringSalida).

pregCompUserToString([],_,_,String,String).
pregCompUserToString([[LPC_XX,[LPC_XYX,LPC_XYY,LPC_XYZ]]|LPC_Y], LP, LC, String, StringSalida):-
    getPregunta(LP,LPC_XX,Preg),
    git_Universal(Preg,1,1,ID),
    git_Universal(Preg,1,2,Autor),
    git_Universal(Preg,1,5,Contenido),
    git_Universal(Preg,1,6,ListaComentario),
    git_Universal(Preg,1,7,Likes),
    concat(String, "    El id es ", String1),
    concat(String1, ID, String2),
    concat(String2, "\n", String3),
    concat(String3, "    El autor es ", String4),
    concat(String4, Autor, String5),
    concat(String5, "\n    ", String6),
    concat(String6, Contenido, String7),
    concat(String7, "\n", String8),
    concat(String8, "    Comentarios: \n", String9),
    comentUserToString(ListaComentario, LC, String9, String10),
    concat(String10, "    Tiene ", String11),
    concat(String11, Likes, String12),
    concat(String12, " likes\n", String13),
    concat(String13, "    Fecha de compartida: ", String14),
    concat(String14, LPC_XYX, String15),
    concat(String15, "/", String16),
    concat(String16, LPC_XYY, String17),
    concat(String17, "/", String18),
    concat(String18, LPC_XYZ, String19),
    concat(String19, "\n---------------------------------------\n", String20),
    pregCompUserToString(LPC_Y, LP, LC, String20, StringSalida).



listStringToString([],String,String).
listStringToString([LS_X|LS_Y], String, StringSalida):-
    concat(String, "    - ", String1),
    concat(String1, LS_X, String2),
    concat(String2, "\n", String3),
    listStringToString(LS_Y, String3, StringSalida).

userToString(Usuario, ListPregunta, ListComentario, Salida):-
    git_Universal(Usuario,1,1,Id),
    git_Universal(Usuario,1,2,Name),
    git_Universal(Usuario,1,3,Pass),
    git_Universal(Usuario,1,5,LP),
    git_Universal(Usuario,1,6,LPC),
    git_Universal(Usuario,1,8,CF),
    git_Universal(Usuario,1,9,CS),
    git_Universal(Usuario,1,10,LF),
    git_Universal(Usuario,1,11,LS),
    pregUserToString(LP, ListPregunta, ListComentario, "", StringLP),
    pregCompUserToString(LPC, ListPregunta, ListComentario, "", StringLC),
    listStringToString(LF, "", StringLF),
    listStringToString(LS, "", StringLS),
    concat("", "El id es ", String),
    concat(String, Id, String1),
    concat(String1, "\n", String2),
    concat(String2, "El nombre es ", String3),
    concat(String3, Name, String4),
    concat(String4, "\n", String5),
    concat(String5, "La contrasena es ", String6),
    concat(String6, Pass, String7),
    concat(String7, "\n", String8),
    concat(String8, "Preguntas hechas: \n", String9),
    concat(String9, StringLP, String10),
    concat(String10, "\n", String11),
    concat(String11, "Preguntas compartidas: \n", String12),
    concat(String12, StringLC, String13),
    concat(String13, "\n", String14),
    concat(String14, "Cuentas seguidas son una cantidad de ", String15),
    concat(String15, CF, String16),
    concat(String16, "\n", String17),
    concat(String17, "Y son: \n", String18),
    concat(String18, StringLF, String19),
    concat(String19, "\n", String20),
    concat(String20, "Cuentas que te siguen son una cantidad de ", String21),
    concat(String21, CS, String22),
    concat(String22, "\n", String23),
    concat(String23, "Y son: \n", String24),
    concat(String24, StringLS, String25),
    concat(String25, "\n", String26),
    Salida = String26,
    true, !.

listUserToString([],_,_,String,String).
listUserToString([LU_X|LU_Y],LP,LC,String,StringSalida):-
    userToString(LU_X, LP, LC, StringUser),
    concat(String, StringUser, String1),
    concat(String1,"\n------------------------\n", String2),
    listUserToString(LU_Y,LP,LC,String2,StringSalida).

% ToString

socialNetworkToString(SN, _):-
    not(isSocialNetwork(SN)),
    false, !.
socialNetworkToString(SN, Salida):-
    git_Universal(SN,1,3,SLU),
    git_Universal(SN,1,4,SLP),
    git_Universal(SN,1,5,SLC),
    existeActivo(SLU),
    cuentaActivada(SLU, User),
    userToString(User, SLP, SLC, Salida),
    true, !.
socialNetworkToString(SN,Salida):-
    git_Universal(SN,1,1,SName),
    git_Universal(SN,1,2,[Dia,Mes,Ano]),
    git_Universal(SN,1,3,SLU),
    git_Universal(SN,1,4,SLP),
    git_Universal(SN,1,5,SLC),
    listUserToString(SLU,SLP,SLC, "", StringUsers),
    concat("Nombre de la social network: ", SName, String),
    concat(String, "\n", String1),
    concat(String1, "Fecha de creacion: ", String2),
    concat(String2, Dia, String3),
    concat(String3, "/", String4),
    concat(String4, Mes, String5),
    concat(String5, "/", String6),
    concat(String6, Ano, String7),
    concat(String7, "\n", String8),
    concat(String8, "Lista Usuarios: \n", String9),
    concat(String9, StringUsers, String10),
    concat(String10, "\n", String11),
    Salida = String11,
    true, !.


% Comentar

socialNetworkComment(SN,Fecha,IDPost,IDComent,TextComent,_):-
    not(isSocialNetwork(SN));
    not(isListaInteger(Fecha));
    not(integer(IDPost));
    not(integer(IDComent));
    not(string(TextComent));
    false,!.
socialNetworkComment(SN,Fecha,IDPost,IDComent,TextComent,SNSalida):-
    IDComent == 0,
    git_Universal(SN,1,3,SLU),
    cuentaActivada(SLU, User),
    git_Universal(User,1,2,Autor),
    agregarComentario(SN, Fecha,TextComent,Autor,[NewSN, IDSalida]),
    agregarComentarioPregunta(NewSN, IDPost, IDSalida, SNOut_pre),
    git_Universal(SNOut_pre,1,3,ListUsers2),
    setDesconectado(ListUsers2, [],NewListUser),
    git_Universal(SNOut_pre,1,1,Lout1),
    git_Universal(SNOut_pre,1,2,Lout2),
    git_Universal(SNOut_pre,1,4,Lout4),
    git_Universal(SNOut_pre,1,5,Lout5),
    SNSalida = [Lout1,Lout2, NewListUser, Lout4, Lout5],
    true, !.
socialNetworkComment(SN,Fecha,IDPost,IDComent,TextComent,SNSalida):-
    git_Universal(SN,1,3,SLU),
    cuentaActivada(SLU, User),
    git_Universal(User,1,2,Autor),
    agregarComentario(SN, Fecha,TextComent,Autor,[NewSN, IDSalida]),
    agregarComentarioComentario(NewSN, IDPost, IDComent, IDSalida, SNOut_pre),
    git_Universal(SNOut_pre,1,3,ListUsers2),
    setDesconectado(ListUsers2, [],NewListUser),
    git_Universal(SNOut_pre,1,1,Lout1),
    git_Universal(SNOut_pre,1,2,Lout2),
    git_Universal(SNOut_pre,1,4,Lout4),
    git_Universal(SNOut_pre,1,5,Lout5),
    SNSalida = [Lout1,Lout2, NewListUser, Lout4, Lout5],
    true, !.

% Like


socialNetworkLike(SN, Fecha, PostID, CommentID, _):-
    not(isSocialNetwork(SN));
    not(isListaInteger(Fecha));
    not(integer(PostID));
    not(integer(CommentID));
    false, !.
socialNetworkLike(SN,_,PostID,CommentID,SNSalida):-
    CommentID == 0,
    git_Universal(SN,1,1,SNombre),
    git_Universal(SN,1,2,SFecha),
    git_Universal(SN,1,3,SLU),
    git_Universal(SN,1,4,SLP),
    git_Universal(SN,1,5,SLC),
    getPregunta(SLP, PostID, Preg),
    git_Universal(Preg,1,1,P1),
    git_Universal(Preg,1,2,P2),
    git_Universal(Preg,1,3,P3),
    git_Universal(Preg,1,4,P4),
    git_Universal(Preg,1,5,P5),
    git_Universal(Preg,1,6,P6),
    git_Universal(Preg,1,7,P7),
    git_Universal(Preg,1,8,P8),
    Total is P7 + 1,
    publicacion(P1,P2,P3,P4,P5,P6,Total,P8, NewPreg),
    cambiar(SLP, NewPreg, [], NewLPreg),
    SNOut_pre = [SNombre,SFecha,SLU,NewLPreg, SLC],
    git_Universal(SNOut_pre,1,3,ListUsers2),
    setDesconectado(ListUsers2, [],NewListUser),
    git_Universal(SNOut_pre,1,1,Lout1),
    git_Universal(SNOut_pre,1,2,Lout2),
    git_Universal(SNOut_pre,1,4,Lout4),
    git_Universal(SNOut_pre,1,5,Lout5),
    SNSalida = [Lout1,Lout2, NewListUser, Lout4, Lout5],
    true, !.
socialNetworkLike(SN,_,PostID,CommentID,SNSalida):-
    git_Universal(SN,1,1,SNombre),
    git_Universal(SN,1,2,SFecha),
    git_Universal(SN,1,3,SLU),
    git_Universal(SN,1,4,SLP),
    git_Universal(SN,1,5,SLC),
    getPregunta(SLP, PostID, Preg),
    git_Universal(Preg,1,6,P6),
    existeComentario(P6, CommentID),
    getComentario(SLC, CommentID, Coment),
    git_Universal(Coment,1,1,C1),
    git_Universal(Coment,1,2,C2),
    git_Universal(Coment,1,3,C3),
    git_Universal(Coment,1,4,C4),
    git_Universal(Coment,1,5,C5),
    git_Universal(Coment,1,6,C6),
    Total is C5 + 1,
    comentario(C1,C2,C3,C4,Total,C6, NewComent),
    cambiar(SLC, NewComent, [], NewLComent),
    SNOut_pre = [SNombre,SFecha,SLU,SLP, NewLComent],
    git_Universal(SNOut_pre,1,3,ListUsers2),
    setDesconectado(ListUsers2, [],NewListUser),
    git_Universal(SNOut_pre,1,1,Lout1),
    git_Universal(SNOut_pre,1,2,Lout2),
    git_Universal(SNOut_pre,1,4,Lout4),
    git_Universal(SNOut_pre,1,5,Lout5),
    SNSalida = [Lout1,Lout2, NewListUser, Lout4, Lout5],
    true, !.



