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

Partial Public Class IPR
    Public Property IDIPR As String
    Public Property IDMODPOZO As String
    Public Property AOF As Double
    Public Property QL As Double
    Public Property PWF As Double
    Public Property ENDRECORD As String

    Public Overridable Property IPR_GENERAL As ICollection(Of IPR_GENERAL) = New HashSet(Of IPR_GENERAL)
    Public Overridable Property MOD_POZO As MOD_POZO

End Class
