Imports ModeloProsper.Prosper
Public Class Pvt
    Public GOR As Double                  ' Relación Gas_Aceite m3/m3
    Public API As Double                  ' Gravedad API
    Public Drg As Double                  ' Densidad relativa del gas producido
    Public Salinidad As Double            ' Salinidad del agua producida en ppm
    Public H2S As Double                  ' Porcentaje de H2S en el gas producido
    Public CO2 As Double                  ' Porcentaje de CO2 en el gas producido
    Public N2 As Double                   ' Porcentaje de N2 en el gas producido
    Public IcorPRB As Integer             ' 
    Public IcorVis As Integer
    Public Tpvt As Double
    Public Psat As Double
    Public Pprueba As Double()
    Public Rs As Double()
    Public Bo As Double()
    Public Muo As Double()

    Property ProgramPath As String
    Property ArchivoPVT As String


    Public Prueba_PVT As New List(Of ParametrosPVT)
    Sub New(ByVal ProgramPath As String, ByVal ArchivoPVT As String)
        Me.ArchivoPVT = ArchivoPVT
        Me.ProgramPath = ProgramPath
    End Sub
    Sub New()

    End Sub
    Function Execute() As Boolean
        Try


            DoSet("PROSPER.PVT.Input.Solgor", GOR)
            DoSet("PROSPER.PVT.Input.Api", API)
            DoSet("PROSPER.PVT.Input.Grvgas", Drg)
            DoSet("PROSPER.PVT.Input.Watsal", Salinidad)
            DoSet("PROSPER.PVT.Input.H2s", H2S)
            DoSet("PROSPER.PVT.Input.Co2", CO2)
            DoSet("PROSPER.PVT.Input.N2", N2)
            DoSet("PROSPER.PVT.Match.Data[0][0][0]", Tpvt)
            DoSet("PROSPER.PVT.Match.Data[0][0][2]", Psat)

            Dim nset As Double = (UBound(Pprueba) + 1) / 15
            Dim Separador = Split(CStr(nset), ".")
            Dim nint As Integer = Separador(0)


            For i = 0 To UBound(Pprueba)
                Dim objetoPrueba As New ParametrosPVT
                objetoPrueba.Presion = Pprueba(i)
                objetoPrueba.RGA = Rs(i)
                objetoPrueba.Bo = Bo(i)
                objetoPrueba.Muo = Muo(i)
                Prueba_PVT.Add(objetoPrueba)
            Next i

            Dim abajopb = (From abajo In Prueba_PVT
                           Where abajo.Presion < Psat
                           Select abajo).ToList()
            Dim arribapb = (From arriba In Prueba_PVT
                            Where arriba.Presion >= Psat
                            Select arriba).ToList()

            Dim Ndatosabojo As Integer = abajopb.LongCount
            Dim Ndatosarriba As Integer = arribapb.LongCount

            Dim ndatos As Integer
            Dim Paux As Double()
            Dim RGAaux As Double()
            Dim Boaux As Double()
            Dim Muoaux As Double()

            If Ndatosabojo >= 10 Then
                Dim sec_dato As Integer = Ndatosabojo / 10
                Dim secaux = Split(CStr(sec_dato), ".")
                Dim Sec_datoaux As Integer = secaux(0)
                ReDim Paux(9)
                ReDim RGAaux(9)
                ReDim Boaux(9)
                ReDim Muoaux(9)

                ndatos = 9
                For i = 0 To 9
                    Paux(9 - i) = abajopb(Ndatosabojo - i * Sec_datoaux - 1).Presion
                    RGAaux(9 - i) = abajopb(Ndatosabojo - i * Sec_datoaux - 1).RGA
                    Boaux(9 - i) = abajopb(Ndatosabojo - i * Sec_datoaux - 1).Bo
                    Muoaux(9 - i) = abajopb(Ndatosabojo - i * Sec_datoaux - 1).Muo
                Next i
            Else
                ReDim Paux(Ndatosabojo - 1)
                ReDim RGAaux(Ndatosabojo - 1)
                ReDim Boaux(Ndatosabojo - 1)
                ReDim Muoaux(Ndatosabojo - 1)
                ndatos = Ndatosabojo - 1
                For i = 0 To Ndatosabojo - 1
                    Paux(i) = abajopb(i).Presion
                    RGAaux(i) = abajopb(i).RGA
                    Boaux(i) = abajopb(i).Bo
                    Muoaux(i) = abajopb(i).Muo
                Next i
            End If
            If Ndatosarriba >= 5 Then
                Dim sec_dato As Double = Ndatosarriba / 5
                Dim secaux = Split(CStr(sec_dato), ".")
                Dim Sec_datoaux As Integer = secaux(0)
                ReDim Preserve Paux(ndatos + 5)
                ReDim Preserve RGAaux(ndatos + 5)
                ReDim Preserve Boaux(ndatos + 5)
                ReDim Preserve Muoaux(ndatos + 5)
                For i = 1 To 5
                    Paux(ndatos + i) = arribapb((i - 1) * Sec_datoaux).Presion
                    RGAaux(ndatos + i) = arribapb((i - 1) * Sec_datoaux).RGA
                    Boaux(ndatos + i) = arribapb((i - 1) * Sec_datoaux).Bo
                    Muoaux(ndatos + i) = arribapb((i - 1) * Sec_datoaux).Muo
                Next i
            Else
                ReDim Preserve Paux(ndatos + Ndatosarriba)
                ReDim Preserve RGAaux(ndatos + Ndatosarriba)
                ReDim Preserve Boaux(ndatos + Ndatosarriba)
                ReDim Preserve Muoaux(ndatos + Ndatosarriba)

                For i = 1 To Ndatosarriba
                    Paux(ndatos + i) = arribapb(i - 1).Presion
                    RGAaux(ndatos + i) = arribapb(i - 1).RGA
                    Boaux(ndatos + i) = arribapb(i - 1).Bo
                    Muoaux(ndatos + i) = arribapb(i - 1).Muo
                Next i
            End If

            For i = 0 To UBound(Paux)
                DoSet("PROSPER.PVT.Match.Data[0][" + i.ToString() + "][0]", Tpvt)
                DoSet("PROSPER.PVT.Match.Data[0][" + i.ToString() + "][1]", Paux(i))
                DoSet("PROSPER.PVT.Match.Data[0][" + i.ToString() + "][2]", Psat)
                DoSet("PROSPER.PVT.Match.Data[0][" + i.ToString() + "][4]", Boaux(i))
                DoSet("PROSPER.PVT.Match.Data[0][" + i.ToString() + "][5]", Muoaux(i))
                DoSet("PROSPER.PVT.Match.Data[0][" + i.ToString() + "][11]", RGAaux(i))
            Next i

            ' DoCmd("PROSPER.PVT.MATCH")
            DoCmd("PROSPER.PVT.MATCHALL")

            'Selección de la Correlación para Pb, Bo, Viscosidad del aceite
            '0   Glaso
            '1   Standing
            '2   Lasater
            '3   Vazquez-Beggs
            '4   Petrosky et al
            '5   Al-Marhoun
            Dim indicador(5) As Double
            Dim numero As Double = 1000000000000000

            For j = 0 To 5
                Dim a1 As Double
                Dim a2 As Double
                Dim a3 As Double
                a1 = Math.Abs(DoGet("PROSPER.PVT.Correl.OilBubpnt[" & CStr(j) & "].F[0]") * DoGet("Prosper.PVT.Correl.OilBubpnt[" & CStr(j) & "].F[1]"))
                a2 = DoGet("PROSPER.PVT.Correl.OilSolgor[" & CStr(j) & "].Sd")
                a3 = DoGet("PROSPER.PVT.Correl.OilOilfvf[" & CStr(j) & "].Sd")
                indicador(j) = a1 + a2 + a3

                If indicador(j) < numero Then IcorPRB = j : numero = indicador(j)
            Next j

            'Selección de la correlación de Viscosidad
            '0   Beal et al
            '1   Beggs et al
            '2   Petrosky et al
            '3   Egbogah et al
            '4   Bergman-Sutton
            ReDim indicador(4)
            numero = 1000000000000000
            For j = 0 To 4
                indicador(j) = DoGet("PROSPER.PVT.Correl.OilOilvis[" & CStr(j) & "].Sd")
                If indicador(j) < numero Then IcorVis = j : numero = indicador(j)
            Next j
            DoSet("PROSPER.PVT.Input.PBcorr", IcorPRB)
            DoSet("PROSPER.PVT.Input.UOcorr", IcorVis)



            Return True
            'Disconnect()
        Catch ex As Exception
            'Disconnect()
            Throw New Exception(ex.Message)
        End Try
    End Function
End Class


Public Class ParametrosPVT
    Property Presion As Double
    Property RGA As Double
    Property Bo As Double
    Property Muo As Double
End Class
