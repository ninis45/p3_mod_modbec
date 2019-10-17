Imports ModeloCI
Public Class Mecanico
    Private Altura As Double
    Private MaxTr As Double = 0
    Private MaxTp As Double = 0
    Private Tuberias As New List(Of MOD_POZO_TUBERIA)
    Private Cima As MOD_POZO_TUBERIA
    Private Tr_remove As Integer

    Private ComponentsTP() As String = {"TUBERIA DE PRODUCCION", "VALVULA DE TORMENTA"}
    Private ComponentsTR() As String = {"TUBERIA DE REVESTIMIENTO"}
    Private TiposTuberias As Dictionary(Of Integer, String)

    Sub New(ByVal db As Entities_ModeloCI, ByVal IdAgujero As String)
        Dim trs = db.VW_TR.Where(Function(w) w.IDAGUJERO = IdAgujero).OrderByDescending(Function(o) o.PROFUNDIDADFIN).ToList()
        Dim tps = db.VW_TP.Where(Function(w) w.IDAGUJERO = IdAgujero).OrderByDescending(Function(o) o.PROFUNDIDAD).ToList()

        'TIPOS TUBERIAS
        '=============================================================================================================================================
        '0.- No aplica
        '1.- TP
        '2.- VT
        '3.- Restricción
        '4.- TR
        TiposTuberias = db.CAT_TIPO_TUBERIA.Where(Function(w) w.ENDRECORD Is Nothing).ToDictionary(Function(d) d.NUMERO, Function(d) d.IDTIPOTUBERIA)

        'MaxTp = tps(0).PROFUNDIDAD
        'MaxAltura = trs(0).PROFUNDIDADFIN ' AGUJERO DESCUBIERTO


        If tps.Count = 0 Then
            Throw New Exception("No hay tps registrados")
        End If


        For i = 0 To tps.Count - 1
            If tps(i).COMPONENTE.Contains("CAPSULA") Then
                MaxTp = tps(i).PROFUNDIDAD

                tps.RemoveAt(i)
                Exit For
            End If
        Next i

        'En caso de no haber capsula buscamos la maxima profundidad
        '===============================================================
        If MaxTp = 0 Then
            MaxTp = tps(0).PROFUNDIDAD
        End If




        ''Buscamos la cima más profunda
        ''================================================================================================
        Dim disparos = (From d In trs Where d.COMPONENTE.Contains("DISPARO") Or d.COMPONENTE.Contains("AGUJERO DESCUBIERTO")).OrderBy(Function(o) o.PROFUNDIDADFIN).ToList()

        'Revisar por intersecciones de disparos dentro del Agujero Descubierto
        If disparos.Count > 0 Then
            AddTope(disparos(0).PROFUNDIDADINICIO, disparos(0).PROFUNDIDADFIN, IIf(disparos(0).COMPONENTE.Contains("AGUJERO DESCUBIERTO"), disparos(0).COMPONENTE, disparos(0).NOMBRE), TiposTuberias(4), IIf(disparos(0).DIAMINT Is Nothing, disparos(0).DIAMEXT, disparos(0).DIAMINT))
        Else
            AddTope(trs(0).PROFUNDIDADINICIO, trs(0).PROFUNDIDADFIN, trs(0).TITULO & " " & trs(0).NOMBRE, TiposTuberias(4), IIf(trs(0).DIAMINT Is Nothing, trs(0).DIAMEXT, trs(0).DIAMINT))
        End If



        If trs.Count > 0 Then
            ' Omitimos la primera posicion por agujero descubierto: i = 1
            ' ==============================================================
            For i = 1 To trs.Count - 1

                If trs(i).COMPONENTE.Contains("TUBERIA") = False Then
                    Continue For
                End If

                'If trs(i).PROFUNDIDADFIN > MaxTr Then
                '    Continue For
                'End If



                Tuberias.Add(New MOD_POZO_TUBERIA() With {
                            .IDTIPOTUBERIA = TiposTuberias(4),
                            .CIDIAM = trs(i).DIAMINT, ' Diametro interior de la tuberia de revestimiento
                            .CIROUG = 0.0006,         ' Rugosidad del interior de la tuberia de revestimiento
                            .TIDIAM = 0,              ' Diametro interior de la tuberia de produccion
                            .TIROUG = 0,              ' Rugosidad del interior de la tuberia de produccion
                            .TODIAM = 0,              ' Diametro exterior de la tuberia de produccion
                            .TOROUG = 0,              ' Rugosidad del exterior de la tuberia de produccion
                            .MD = trs(i - 1).PROFUNDIDADINICIO,
                            .ETIQUETA = trs(i).TITULO
                    })
                If trs(i).PROFUNDIDADINICIO = 0 Then
                    Exit For
                End If


            Next
        Else
            Throw New Exception("No existen equipamiento TR")
        End If


        Dim tps_temp = (From tp In tps Where tp.PROFUNDIDAD <= MaxTp And ComponentsTP.Contains(tp.COMPONENTE)).ToList()
        For i = 0 To tps_temp.Count - 1

            'Buscar mayores de profundidad inicio y menores de profundidad fin
            '===================================================================
            Dim tr_active = (From tub In trs Where tub.PROFUNDIDADINICIO <= tps_temp(i).PROFUNDIDAD And tub.PROFUNDIDADFIN > tps_temp(i).PROFUNDIDAD).OrderByDescending(Function(o) o.PROFUNDIDADFIN).ToList()


            Dim label = IIf(tps_temp(i).COMPONENTE.Contains("E.M.R."), "E.M.R.", "TP " & tps_temp(i).DIAMINT.ToString())
            Dim cidiam As Double = tr_active(0).DIAMINT
            Dim ciroug As Double = 0.0006
            Dim tidiam As Double = tps_temp(i).DIAMINT
            Dim tiroug As Double = 0.0006
            Dim todiam As Double = tps_temp(i).DIAMEXT
            Dim toroug As Double = 0.0006
            Dim md As Double = tps_temp(i).PROFUNDIDAD
            Dim tuberia = TiposTuberias(1)


            Dim ini As Double = 0, fin As Double = 0


            If tps_temp.Count - 1 > i Then
                ini = tps_temp(i + 1).PROFUNDIDAD
            End If



            fin = tps_temp(i).PROFUNDIDAD



            If tr_active.Count > 0 Then


                If tps_temp(i).COMPONENTE.Contains("VALVULA") Then

                    md = md
                    cidiam = 0
                    ciroug = 0
                    tiroug = 0
                    todiam = 0
                    toroug = 0
                    label = "SSSV"
                    tuberia = TiposTuberias(2)
                End If

                Tuberias.Add(New MOD_POZO_TUBERIA() With {
                    .IDTIPOTUBERIA = tuberia,
                    .CIDIAM = cidiam,
                    .CIROUG = ciroug,
                    .TIDIAM = tidiam,
                    .TIROUG = tiroug,
                    .TODIAM = todiam,
                    .TOROUG = toroug,
                    .MD = md,
                    .ETIQUETA = label
                })
            Else
                Throw New Exception("No hay TRs en el nivel de profundidad: " & label)
            End If
            'Buscamos cortes o cambios en Diametro de TR
            '==========================================================
            If tps(i).COMPONENTE.Contains("TUBERIA") Then
                Dim Cut = Cuts(ini, fin)
                If Cut IsNot Nothing Then
                    Tuberias.Add(New MOD_POZO_TUBERIA() With {
                        .IDTIPOTUBERIA = TiposTuberias(1),
                        .CIDIAM = Cut.CIDIAM,
                        .CIROUG = 0.0006,
                        .TIDIAM = tps_temp(i).DIAMINT,
                        .TIROUG = 0.0006,
                        .TODIAM = tps_temp(i).DIAMEXT,
                        .TOROUG = 0.0006,
                        .MD = Cut.MD,
                        .ETIQUETA = "TP " & tps_temp(i).DIAMINT
                    })
                End If
            End If

        Next i

        'Eliminacion de Trs absorbidas por Tps en caso de sobre pasar la profundidad
        Dim total_tubs = Tuberias.Count - 1
        While total_tubs > 0
            If Tuberias(total_tubs).IDTIPOTUBERIA = TiposTuberias(4) Then

                If Tuberias(total_tubs).MD < MaxTp Then
                    Tuberias.RemoveAt(total_tubs)

                End If
            End If
            total_tubs -= 1
        End While

    End Sub

    Sub AddTope(ByVal prof_ini As Double, ByVal prof_fin As Double, ByVal label As String, ByVal tipo_tr As String, Optional ByVal cid As Double = 0)



        Dim md As Double = prof_fin

        If prof_fin > prof_ini Then
            md = ((prof_fin - prof_ini) / 2) + prof_ini
        End If

        Dim tub = New MOD_POZO_TUBERIA() With {
                    .IDTIPOTUBERIA = TiposTuberias(4),
                    .CIDIAM = cid, ' Diametro interior de la tuberia de revestimiento
                    .CIROUG = 0.0006,         ' Rugosidad del interior de la tuberia de revestimiento
                    .TIDIAM = 0,              ' Diametro interior de la tuberia de produccion
                    .TIROUG = 0,              ' Rugosidad del interior de la tuberia de produccion
                    .TODIAM = 0,              ' Diametro exterior de la tuberia de produccion
                    .TOROUG = 0,              ' Rugosidad del exterior de la tuberia de produccion
                    .MD = md,
                    .ETIQUETA = label
        }

        If Tuberias.Count > 0 Then
            Tuberias(0) = tub
        Else

            Tuberias.Add(tub)
        End If

        MaxTr = prof_fin

    End Sub
    Function GetList() As List(Of MOD_POZO_TUBERIA)
        Dim list = Tuberias.OrderBy(Function(o) o.MD).ToList()

        If list(0).ETIQUETA.Contains("E.M.R.") = False Then
            list.Add(New MOD_POZO_TUBERIA() With {
                .ETIQUETA = "Xtree",
                .CIDIAM = 0,
                .MD = 0,
                .TIDIAM = 0,
                .TIROUG = 0,
                .IDTIPOTUBERIA = TiposTuberias(0),
                .TODIAM = 0,
                .TOROUG = 0,
                .CIROUG = 0
            })
        End If

        Return list

    End Function
    Function Cuts(ByVal ini As Double, ByVal fin As Double) As MOD_POZO_TUBERIA

        Dim result = (From tub In Tuberias Where tub.TIDIAM = 0 And tub.MD > ini And tub.MD <= fin).OrderByDescending(Function(o) o.MD).ToList()

        If result.Count > 0 Then



            Return result(0)
        End If


        Return Nothing

    End Function



    Sub Initialize()

    End Sub


End Class
