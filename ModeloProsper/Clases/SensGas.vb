Imports ModeloProsper.Prosper

Public Class SensGas
    Property Server As Object
    Property ProgramPath As String
    Property ArchivoPVT As String

#Region "Propiedades"
    'Shared Server As Object
    'Shared AppName As String
    'Shared connected As Boolean

    'Property Archivo As String = "C:\Users\97029592\Desktop\Modelos pozos de Maloob\Maloob-50.Out"

    Property Pwh As Double
    Property Wc As Double
    Property GOR_Total As Double
    Property Qgi_Min As Double
    Property Qgi_Max As Double
    Property Qgi_vec As Double()
    Property Ql_vec As Double()

#End Region

    Sub New()

    End Sub
    Function Execute() As Boolean
        Try
            Me.Server = Connect(ProgramPath, ArchivoPVT)

            ReDim Qgi_vec(19)
            ReDim Ql_vec(19)


            'Dim prosper = Shell("C:\Program Files (x86)\Petroleum Experts\IPM 11\prosper.EXE", 1)
            ' Server = CreateObject("PX32.OpenServer.1")

            ''AppName = "PROSPER"
            'DoCmd("PROSPER.OPENFILE(""" + Archivo + """)")

            DoCmd("PROSPER.SETUNITSYS(""Pemex"")")

            DoSet("PROSPER.ANL.SYS.Pres", Pwh)
            DoSet("PROSPER.ANL.SYS.WC", Wc)
            DoSet("PROSPER.ANL.SYS.GOR", GOR_Total)
            DoSet("PROSPER.ANL.SYS.SolutionNode", 1)
            DoSet("PROSPER.ANL.SYS.RateMethod", 0)
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Clear", "")
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[0]", 22)
            Dim incre As Double = (Qgi_Max - Qgi_Min) / 19
            For i = 0 To 19
                Qgi_vec(i) = Qgi_Min + incre * i
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[138].Vals[" & CStr(i) & "]", Qgi_vec(i))

            Next i
            DoCmd("PROSPER.ANL.SYS.CALC")

            Dim Results = Integer.Parse(DoGet("PROSPER.OUT.SYS.Results.COUNT"))

            If Results = 0 Then
                Throw New Exception("No hubo resultados en la operacion: PROSPER.OUT.SYS.Results")
            End If

            For i = 0 To Results - 1
                Ql_vec(i) = DoGet("PROSPER.OUT.SYS.Results[" + i.ToString() + "].Sol.LiqRate")
            Next i
            Disconnect()
            Return True
        Catch ex As Exception
            Disconnect()
            Throw New Exception(ex.Message)
        End Try


    End Function

End Class
