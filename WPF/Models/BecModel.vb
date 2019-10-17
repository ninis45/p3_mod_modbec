Imports ModeloCI
Public Class BecModel
    Private db As New Entities_ModeloCI()
    Sub New(ByVal db As Entities_ModeloCI)
        Me.db = db

    End Sub
    Function Save(ByVal IdModPozo As String, ByVal ModBec As MOD_POZO_BEC)
        Try
            Dim NewBec = db.MOD_POZO_BEC.Where(Function(w) w.IDMODPOZO = IdModPozo).SingleOrDefault()

            If NewBec Is Nothing Then
                NewBec = New MOD_POZO_BEC() With {
                        .IDMODPOZO = IdModPozo,
                        .IDMODPOZOBEC = Guid.NewGuid().ToString().ToUpper(),
                        .CABLE_BEC = ModBec.CABLE_BEC,
                        .PRESUC_BEC = ModBec.PRESUC_BEC,
                        .CORRIENTE_BEC = ModBec.CORRIENTE_BEC,
                        .POTENCIA_BEC = ModBec.POTENCIA_BEC,
                        .PREDES_BEC = ModBec.PREDES_BEC,
                        .BOMBA_BEC = ModBec.BOMBA_BEC,
                        .DESGASTE_BEC = ModBec.DESGASTE_BEC,
                        .EFISEPGAS_BEC = ModBec.EFISEPGAS_BEC,
                        .ETAPAS_BEC = ModBec.ETAPAS_BEC,
                        .FRECMAX = ModBec.FRECMAX,
                        .FRECMIN = ModBec.FRECMIN,
                        .FREC_BEC = ModBec.FREC_BEC,
                        .LONGCABLE_BEC = ModBec.LONGCABLE_BEC,
                        .MOTOR_BEC = ModBec.MOTOR_BEC,
                        .ODMAX_BEC = ModBec.ODMAX_BEC,
                        .POTENCIAMOTOR_BEC = ModBec.POTENCIAMOTOR_BEC,
                        .PROF_BEC = ModBec.PROF_BEC,
                        .REDUCGAS_BEC = ModBec.REDUCGAS_BEC,
                        .VOLTSUP_BEC = ModBec.VOLTSUP_BEC
                }
                db.MOD_POZO_BEC.Add(NewBec)
            Else

                db.Entry(ModBec).State = System.Data.Entity.EntityState.Modified
            End If
            db.SaveChanges()
            Return True
        Catch ex As Exception
            MsgBox(ex.Message)
            Return False
        End Try


    End Function
    Function Add()

    End Function
    Function Edit()
    End Function
End Class
