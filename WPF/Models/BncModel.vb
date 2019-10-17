Imports ModeloCI
Public Class BncModel
    Private db As New Entities_ModeloCI()
    Sub New(ByVal db As Entities_ModeloCI)
        Me.db = db

    End Sub

    Function Save(ByVal IdModPozo As String, ByVal ModBnc As MOD_POZO_BNC) As Boolean
        Try
            Dim Bnc = db.MOD_POZO_BNC.Where(Function(w) w.IDMODPOZO = IdModPozo).SingleOrDefault()

            If Bnc Is Nothing Then
                Bnc = New MOD_POZO_BNC() With {
                    .CO2 = ModBnc.CO2,
                    .DIAMVAL = ModBnc.DIAMVAL,
                    .ENTRY = ModBnc.ENTRY,
                    .GLRATE = ModBnc.GLRATE,
                    .GLRINY = ModBnc.GLRINY,
                    .GRAVITY = ModBnc.GRAVITY,
                    .H2S = ModBnc.H2S,
                    .IDMODPOZO = IdModPozo,
                    .METHOD = ModBnc.METHOD,
                    .N2 = ModBnc.N2,
                    .QGIMAX = ModBnc.QGIMAX,
                    .QGIMIN = ModBnc.QGIMIN,
                    .RANGESYSTEM = ModBnc.RANGESYSTEM,
                    .VALVEDEPTH = ModBnc.VALVEDEPTH
                }
                db.MOD_POZO_BNC.Add(Bnc)

            Else
                db.Entry(ModBnc).State = System.Data.Entity.EntityState.Modified
            End If
            db.SaveChanges()
            Return True
        Catch ex As Exception
            Return False
        End Try


    End Function
End Class
