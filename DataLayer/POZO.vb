'------------------------------------------------------------------------------
' <auto-generated>
'     Este código se generó a partir de una plantilla.
'
'     Los cambios manuales en este archivo pueden causar un comportamiento inesperado de la aplicación.
'     Los cambios manuales en este archivo se sobrescribirán si se regenera el código.
' </auto-generated>
'------------------------------------------------------------------------------

Imports System
Imports System.Collections.Generic

Partial Public Class POZO
    Public Property IDPOZO As String
    Public Property IDMACROPERA As String
    Public Property IDCATSAP As Integer
    Public Property NOMBRE As String
    Public Property COORDENADAX As Nullable(Of Double)
    Public Property COORDENADAY As Nullable(Of Double)
    Public Property ESTADO As Nullable(Of Boolean)
    Public Property EMR As String
    Public Property TIRANTEAGUA As String
    Public Property RESUMEN As String
    Public Property IDCAMPO As String
    Public Property IIP As String
    Public Property ENDRECORD As String

    Public Overridable Property AGUJERO As ICollection(Of AGUJERO) = New HashSet(Of AGUJERO)

End Class