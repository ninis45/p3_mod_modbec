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

Partial Public Class PRODUCTIVIDAD
    Public Property IDPRODUCTIVIDAD As String
    Public Property WC_RES As Double
    Public Property QGI_RES As Double
    Public Property IDMODPOZO As String
    Public Property ENDRECORD As String

    Public Overridable Property MOD_POZO As MOD_POZO
    Public Overridable Property PRODUCTIVIDAD_DETALLE As ICollection(Of PRODUCTIVIDAD_DETALLE) = New HashSet(Of PRODUCTIVIDAD_DETALLE)

End Class
