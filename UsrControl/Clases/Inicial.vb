Module Datos
    Public ht As New Hashtable
End Module

Public Class Inicial
    Sub New()

    End Sub
    Shared Sub Init(ByVal LiftMethod As Integer)
        ht.Clear()
        ht.Add("COMPANY", "Petroleos Mexicanos")
        ht.Add("FIELD", "EK-BALAN")
        ht.Add("LOCATIONS", "CIUDAD DEL CARMEN")
        ht.Add("WELL", "EK-BALAM")
        ht.Add("PLATAFORM", "EK-BALAM")
        ht.Add("ANALYST", "FDJ")


        Select Case LiftMethod
            Case 1
                ht.Add("PRES", 183.9)
                ht.Add("TRES", 74.87)
                ht.Add("WC", 22.56)
                ht.Add("TOTGOR", 525.15)
                ht.Add("QTEST", 503.62)
                ht.Add("PTEST", 98.33)
            Case 2
                ht.Add("PRES", 262.014)
                ht.Add("TRES", 93.33)
                ht.Add("WC", 30.0)
                ht.Add("TOTGOR", 70.88)
                ht.Add("QTEST", 8290.0)
                ht.Add("PTEST", 164.014)

        End Select






        ht.Add("RESPERM", 150.0)
        ht.Add("THICKNESS", 30.48)
        ht.Add("DRAINAGE", 1375946.0)
        ht.Add("DIETZ", 31.6)
        ht.Add("WBR", 4.248)
        ht.Add("SKIN", 2.0)




        ht.Add("HTC", 7.9375)
        ht.Add("THPD", 17.5775)
        ht.Add("THTD", 70.2778)
        ht.Add("RGATOTALAFORO", 70.88)

        ht.Add("TRPRES", 0.0)


        ht.Add("GastoInyeccion", Nothing) 'DEPRECIADO

        ht.Add("FLUID", 0.0)
        ht.Add("PVTMODEL", 0.0)
        ht.Add("SEPARATOR", 0.0)
        ht.Add("EMULSION", 0.0)
        ht.Add("HYDRATE", 0.0)
        ht.Add("WATVIS", 0.0)
        ht.Add("VISMOD", 0.0)
        ht.Add("FLOWTYPE", 0.0)
        ht.Add("WELLTYPE", 0.0)
        ht.Add("LIFTMETHOD", 1.0)
        ht.Add("LIFTYPE", 1.0)
        ht.Add("PREDICT", 2.0)
        ht.Add("TEMPMODEL", 0.0)

        ht.Add("OUTPUTRES", 0.0)
        ht.Add("COMPLETION", 0.0)
        ht.Add("GRAVELPACK", 0.0)
        ht.Add("INFLOWTYPE", 0.0)
        ht.Add("GASCONING", 0.0)
        ht.Add("DatGenDate", Date.Today)
        ht.Add("Comenta", "")
        ht.Add("IPRMETHOD", 3.0)
        ht.Add("COMPACT", 0.0)
        ht.Add("IRElK", 0.0)
        ht.Add("MGSKINMETHOD", 0.0)
        ht.Add("MGSKINMODEL", 0.0)

        'BNC
        ht.Add("ENTRY", 1.0)
        ht.Add("METHOD", 0.0)
        ht.Add("H2S", 0.0)
        ht.Add("CO2", 0.17)
        ht.Add("N2", 0.31)
        ht.Add("GLRINY", 0.0)
        ht.Add("GLRATE", 0.0)
        ht.Add("VALVEDEPTH", 2484.7)
        ht.Add("DIAMVAL", 20.0)
        ht.Add("QGIMIN", 0.0)
        ht.Add("QGIMAX", 20.0)
        ht.Add("GRAVITY", 0.675)
        ht.Add("RANGESYSTEM", 0.0)
        'BEC
        ht.Add("FRECMIN", 45.0)
        ht.Add("FRECMAX", 70.0)
        ht.Add("PROF", 2133.6)
        ht.Add("FREC", 60.0)
        ht.Add("ODMAX", 6.0)
        ht.Add("LONGCABLE", 2133.6)
        ht.Add("EFISEPGAS", 0.0)
        ht.Add("ETAPAS", 128.0)
        ht.Add("VOLTSUP", 4125.0)
        ht.Add("DESGASTE", 0.0)
        ht.Add("REDUCGAS", 0.0)
        ht.Add("BOMBA", 125.0)
        ht.Add("MOTOR", 4.0)
        ht.Add("POTENCIAMOTOR", 1.0)
        ht.Add("CABLE", 0.0)
        ht.Add("PRESUC", 45.0)
        ht.Add("CORRIENTE", 0.0)
        ht.Add("POTENCIA", 0.0)
        ht.Add("PREDES", 164.014)
    End Sub
End Class
