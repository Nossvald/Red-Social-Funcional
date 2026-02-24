import Test.HUnit
import Solucion

-- Definimos funciones constantes para hacer testing:
u1 = (1,"User1")
uIdem1 = (3,"User1")
u2 = (2,"User2")
u3 = (3,"User3")
u4 = (4,"User4")
u5 = (5,"User5")
u6 = (6,"User6")
u7 = (7,"User7")
u8 = (8,"User8")
u9 = (9,"User9")
u10 = (10,"User10")
u11 = (11,"User11")
u12 = (12,"User12")

testNoUsers = []
testAllUsers = [u1,u2,u3,u4,u5,u6,u7,u8,u9,u11,u12]
testUsers = [u1,u2,u3,u4]
testUsersNombresIguales = [u1,uIdem1,u2]

testColoquio = [(u1,u2)]
testRelEmpty = []
testRelDirecta = [(u1,u4)]
testRelCadena = [(u1,u2), (u3,u2), (u3,u5), (u5,u4)]
testNoRel = [(u1,u2), (u4,u3)]
testSoloU1 = [(u1,u2)]
testSoloU4 = [(u4,u2)]
testRelOrden1 = [(u1,u2), (u3,u1), (u1,u4), (u4,u6), (u1,u5), (u2,u5)]
testRelOrden2 = [(u2,u5), (u4,u1), (u1,u2), (u4,u6), (u1,u3), (u1,u5)]
testColoquioNoRecto = [(u1,u2), (u1,u3), (u2,u3), (u2,u4), (u3,u5)]

testRobertoCarlos = [(u1, u) | u <- [u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12]]

testPubliEmpty = []
testPubli = [(u2, "Buenas", [u1,u2]), (u4, "Cómo están?", []), (u2, "Malas", [u3])]
-- Fin de la definición

main = runTestTT todosLosTest

todosLosTest = test [
    nombresDeUsuariosTestSuite,
    amigosDeTestSuite,
    cantidadDeAmigosTestSuite,
    usuarioConMasAmigosTestSuite,
    estaRobertoCarlosTestSuite,
    publicacionesDeTestSuite,
    publicacionesQueLeGustanATestSuite,
    lesGustanLasMismasPublicacionesTestSuite,
    tieneUnSeguidorFielTestSuite,
    existeSecuenciaDeAmigosTestSuite
    ]

nombresDeUsuariosTestSuite = test [
    "Caso 1: no hay usuarios" ~: nombresDeUsuarios ([],[],[]) ~?= [],
    "Caso 2: hay usuarios" ~: nombresDeUsuarios (testUsers,[],[]) ~?= ["User1", "User2", "User3", "User4"],
    "Caso 3: hay usuarios pero algunos se llaman igual" ~: nombresDeUsuarios (testUsersNombresIguales,[],[]) ~?= ["User1", "User2"]
    ]

amigosDeTestSuite = test [
    "Caso 1: no hay relaciones en la red" ~: amigosDe (testUsers,[],[]) u1 ~?= [],
    "Caso 2: hay relaciones pero u1 no tiene amigos" ~: amigosDe (testUsers,[(u2,u3)],[]) u1 ~?= [],
    "Caso 3: u1 tiene un solo amigo" ~: amigosDe (testUsers,[(u1,u2)],[]) u1 ~?= [u2],
    "Caso 4: hay relaciones y u1 tiene algunos amigos" ~: amigosDe (testUsers,[(u1,u2), (u3,u1), (u4,u3)],[]) u1 ~?= [u2, u3],
    "Caso 5: hay relaciones y todas ellas son con u1" ~: amigosDe (testUsers,[(u1,u2), (u3,u1)],[]) u1 ~?= [u2, u3]
    ]

cantidadDeAmigosTestSuite = test [
    "Caso 1: no hay relaciones en la red" ~: cantidadDeAmigos (testUsers,[],[]) u1 ~?= 0,
    "Caso 2: hay relaciones pero u1 no tiene amigos" ~: cantidadDeAmigos (testUsers,[(u2,u3)],[]) u1 ~?= 0,
    "Caso 3: u1 tiene un solo amigo" ~: cantidadDeAmigos (testUsers,[(u1,u2)],[]) u1 ~?= 1,
    "Caso 4: hay relaciones y u1 tiene algunos amigos" ~: cantidadDeAmigos (testUsers,[(u1,u2), (u3,u1), (u4,u3)],[]) u1 ~?= 2,
    "Caso 5: hay relaciones y todas ellas son con u1" ~: cantidadDeAmigos (testUsers,[(u1,u2), (u3,u1)],[]) u1 ~?= 2
    ]

usuarioConMasAmigosTestSuite = test [
    "Caso 1: nadie tiene amigos" ~: usuarioConMasAmigos (testUsers,[],[]) ~?= u1,
    "Caso 2: dos o más usuarios tienen la misma cantidad de amigos" ~: usuarioConMasAmigos (testUsers,[(u4,u2), (u4,u3), (u2,u3)],[]) ~?= u2,
    "Caso 3: sólo un usuario tiene más amigos" ~: usuarioConMasAmigos (testUsers,[(u1,u2), (u1,u3)],[]) ~?= u1
    ]

estaRobertoCarlosTestSuite = test [
    "Caso 1: nadie tiene amigos" ~: estaRobertoCarlos (testUsers,[],[]) ~?= False,
    "Caso 2: la red es vacía" ~: estaRobertoCarlos ([],[],[]) ~?= False,
    "Caso 3: u1 no tiene más de 10 amigos" ~: estaRobertoCarlos (testUsers,[(u1,u3)],[]) ~?= False,
    "Caso 4: u1 tiene más de 10 amigos" ~: estaRobertoCarlos (testUsers,testRobertoCarlos,[]) ~?= True
    ]

publicacionesDeTestSuite = test [
    "Caso 1: no hay publicaciones en la red" ~: publicacionesDe (testUsers,[],[]) u2 ~?= [],
    "Caso 2: ninguna de las publicaciones son de u2" ~: publicacionesDe (testUsers,[],[(u4, "Cómo están?", [])]) u2 ~?= [],
    "Caso 3: u2 tiene publicaciones" ~: publicacionesDe (testUsers,[],[(u2, "Buenas", [u1,u2]), (u4, "Cómo están?", []), (u2, "Malas", [u3])]) u2 ~?= [(u2, "Buenas", [u1,u2]), (u2, "Malas", [u3])],
    "Caso 4: todas las publicaciones de la red son de u2" ~: publicacionesDe (testUsers,[],testPubli) u2 ~?= [(u2, "Buenas", [u1,u2]), (u2, "Malas", [u3])]
    ]

publicacionesQueLeGustanATestSuite = test [
    "Caso 1: no hay publicaciones en la red" ~: publicacionesQueLeGustanA (testUsers,[],[]) u4 ~?= [],
    "Caso 2: a un usuario (u4) no le gusta nada (es un mala onda)" ~: publicacionesQueLeGustanA (testUsers,[],testPubli) u4 ~?= [],
    "Caso 3: a un usuario (u1) le gusta una o más publicaciones" ~: publicacionesQueLeGustanA (testUsers,[],testPubli) u1 ~?= [(u2, "Buenas", [u1,u2])],
    "Caso 4: a un usuario (u1) le gusta todo" ~: publicacionesQueLeGustanA (testUsers,[],[(u2, "Buenas", [u1,u2]), (u4, "Cómo están?", [u1]), (u2, "Malas", [u3, u1])]) u1 ~?= [(u2, "Buenas", [u1,u2]), (u4, "Cómo están?", [u1]), (u2, "Malas", [u3, u1])]
    ]

lesGustanLasMismasPublicacionesTestSuite = test [
    "Caso 1: no hay publicaciones en la red" ~: lesGustanLasMismasPublicaciones (testUsers,[],[]) u1 u2 ~?= True,
    "Caso 2: a ninguno le gusta ninguna publicación" ~: lesGustanLasMismasPublicaciones (testUsers,[],[(u2, "Buenas", []), (u4, "Cómo están?", [u3]), (u2, "Malas", [])]) u1 u2 ~?= True,
    "Caso 3: a u1 o u2 no le gusta nada" ~: lesGustanLasMismasPublicaciones (testUsers,[],[(u2, "Buenas", []), (u4, "Cómo están?", [u3]), (u2, "Malas", [u1])]) u1 u2 ~?= False,
    "Caso 4: no le gustan las mismas publicaciones a u1 y a u2" ~: lesGustanLasMismasPublicaciones (testUsers,[],[(u2, "Buenas", [u1]), (u4, "Cómo están?", [u3]), (u2, "Malas", [u2,u1])]) u1 u2 ~?= False,
    "Caso 5: le gustan las mismas publicaciones a u1 y u2" ~: lesGustanLasMismasPublicaciones (testUsers,[],[(u2, "Buenas", [u1,u2]), (u4, "Cómo están?", [u3]), (u2, "Malas", [u2,u1])]) u1 u2 ~?= True,
    "Caso 6: diferencia simétrica no vacía" ~: lesGustanLasMismasPublicaciones (testUsers,[],[(u2, "Buenas", [u1]), (u4, "Cómo están?", [u1,u2]), (u2, "Malas", [u2])]) u1 u2 ~?= False
    ]

tieneUnSeguidorFielTestSuite = test [
    "Caso 1: no hay publicaciones en la red" ~: tieneUnSeguidorFiel (testUsers,[],[]) u2 ~?= False,
    "Caso 2: u2 no tiene publicaciones" ~: tieneUnSeguidorFiel (testUsers,[],[(u4, "Cómo están?", [u3])]) u2 ~?= False, -- Este caso lo hicimos aparte por la especificación (res <=> ...)
    "Caso 3: el único seguidor fiel de u2 es si mismo" ~: tieneUnSeguidorFiel (testUsers,[],[(u2, "Buenas", [u2]), (u4, "Cómo están?", [u3]), (u2, "Malas", [u2])]) u2 ~?= False,
    "Caso 4: u2 no tiene ningún seguidor fiel" ~: tieneUnSeguidorFiel (testUsers,[],[(u2, "Buenas", []), (u4, "Cómo están?", [u3]), (u2, "Malas", [])]) u2 ~?= False,
    "Caso 5: u2 tiene algún seguidor fiel distinto a si mismo" ~: tieneUnSeguidorFiel (testUsers,[],[(u2, "Buenas", [u1,u2]), (u4, "Cómo están?", [u3]), (u2, "Malas", [u2,u1])]) u2 ~?= True
    ]

existeSecuenciaDeAmigosTestSuite = test [
    "Caso 1: hay una secuencia directa entre u1 y u4" ~: existeSecuenciaDeAmigos (testUsers,testRelDirecta,[]) u1 u4 ~?= True,
    "Caso 2: hay una secuencia (no directa, cadena) entre u1 y u4" ~: existeSecuenciaDeAmigos (testUsers,testRelCadena,[]) u1 u4 ~?= True,
    "Caso 3: no existe una secuencia de amigos entre u1 y u4." ~: existeSecuenciaDeAmigos (testUsers,testNoRel,[]) u1 u4 ~?= False,
    "Caso 4: no hay relaciones en la red" ~: existeSecuenciaDeAmigos (testUsers,testRelEmpty,[]) u1 u4 ~?= False,
    "Caso 5: u4 no tiene amigos" ~: existeSecuenciaDeAmigos (testUsers,testSoloU1,[]) u1 u4 ~?= False,
    "Caso 6: u1 amigo de si mismo" ~: existeSecuenciaDeAmigos (testUsers,testColoquio,[]) u1 u1 ~?= True,
    "Caso 7: u1 es amigo de u5 pero no es un camino recto (grafo)" ~: existeSecuenciaDeAmigos (testUsers,testColoquioNoRecto,[]) u1 u5 ~?= True,
    -- Los casos de test a continuación fueron realizados tras cambiar el código más de una vez por problemas de orden.
    "Caso 8 (especial): hay relación pero con orden modificado (1)" ~: existeSecuenciaDeAmigos (testAllUsers,testRelOrden1,[]) u1 u6 ~?= True,
    "Caso 9 (especial): hay relación pero con orden modificado (2)" ~: existeSecuenciaDeAmigos (testAllUsers,testRelOrden2,[]) u1 u6 ~?= True
    ]
