﻿<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class grfGas
    Inherits System.Windows.Forms.UserControl

    'UserControl reemplaza a Dispose para limpiar la lista de componentes.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Requerido por el Diseñador de Windows Forms
    Private components As System.ComponentModel.IContainer

    'NOTA: el Diseñador de Windows Forms necesita el siguiente procedimiento
    'Se puede modificar usando el Diseñador de Windows Forms.  
    'No lo modifique con el editor de código.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(grfGas))
        Me.TChart1 = New Steema.TeeChart.TChart()
        Me.Line1 = New Steema.TeeChart.Styles.Line()
        Me.Line2 = New Steema.TeeChart.Styles.Line()
        Me.Line3 = New Steema.TeeChart.Styles.Line()
        Me.Line4 = New Steema.TeeChart.Styles.Line()
        Me.Line5 = New Steema.TeeChart.Styles.Line()
        Me.Line6 = New Steema.TeeChart.Styles.Line()
        Me.Line7 = New Steema.TeeChart.Styles.Line()
        Me.Line8 = New Steema.TeeChart.Styles.Line()
        Me.ChartController1 = New Steema.TeeChart.ChartController()
        Me.SuspendLayout()
        '
        'TChart1
        '
        '
        '
        '
        Me.TChart1.Aspect.ColorPaletteIndex = 20
        Me.TChart1.Aspect.View3D = False
        '
        '
        '
        Me.TChart1.Axes.Automatic = True
        '
        '
        '
        '
        '
        '
        Me.TChart1.Axes.Bottom.Grid.DrawEvery = 2
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TChart1.Axes.Bottom.Labels.Font.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer))
        '
        '
        '
        '
        '
        '
        Me.TChart1.Axes.Bottom.Labels.Font.Shadow.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(147, Byte), Integer), CType(CType(147, Byte), Integer), CType(CType(147, Byte), Integer))
        Me.TChart1.Axes.Bottom.Labels.Font.Size = 9
        Me.TChart1.Axes.Bottom.Labels.Font.SizeFloat = 9.0!
        '
        '
        '
        Me.TChart1.Axes.Bottom.Title.Caption = "Gasto de Liquido(STBPD)"
        '
        '
        '
        '
        '
        '
        Me.TChart1.Axes.Bottom.Title.Font.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(64, Byte), Integer), CType(CType(64, Byte), Integer), CType(CType(64, Byte), Integer))
        '
        '
        '
        Me.TChart1.Axes.Bottom.Title.Font.Shadow.Width = 0
        Me.TChart1.Axes.Bottom.Title.Font.Size = 9
        Me.TChart1.Axes.Bottom.Title.Font.SizeFloat = 9.0!
        Me.TChart1.Axes.Bottom.Title.Lines = New String() {"Gasto de Liquido(STBPD)"}
        '
        '
        '
        '
        '
        '
        Me.TChart1.Axes.Left.Grid.DrawEvery = 2
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TChart1.Axes.Left.Labels.Font.Brush.Color = System.Drawing.Color.Gray
        Me.TChart1.Axes.Left.Labels.Font.Size = 9
        Me.TChart1.Axes.Left.Labels.Font.SizeFloat = 9.0!
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TChart1.Axes.Left.Title.Font.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(64, Byte), Integer), CType(CType(64, Byte), Integer), CType(CType(64, Byte), Integer))
        Me.TChart1.Axes.Left.Title.Font.Size = 11
        Me.TChart1.Axes.Left.Title.Font.SizeFloat = 11.0!
        '
        '
        '
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TChart1.Axes.Right.Labels.Font.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(64, Byte), Integer), CType(CType(64, Byte), Integer), CType(CType(64, Byte), Integer))
        Me.TChart1.Axes.Right.Labels.Font.Size = 9
        Me.TChart1.Axes.Right.Labels.Font.SizeFloat = 9.0!
        '
        '
        '
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TChart1.Axes.Top.Labels.Font.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(64, Byte), Integer), CType(CType(64, Byte), Integer), CType(CType(64, Byte), Integer))
        Me.TChart1.Axes.Top.Labels.Font.Size = 9
        Me.TChart1.Axes.Top.Labels.Font.SizeFloat = 9.0!
        Me.TChart1.CurrentTheme = Steema.TeeChart.ThemeType.Report
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TChart1.Header.Font.Brush.Color = System.Drawing.Color.Gray
        Me.TChart1.Header.Font.Size = 12
        Me.TChart1.Header.Font.SizeFloat = 12.0!
        '
        '
        '
        Me.TChart1.Legend.Alignment = Steema.TeeChart.LegendAlignments.Bottom
        '
        '
        '
        '
        '
        '
        Me.TChart1.Legend.Font.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(64, Byte), Integer), CType(CType(64, Byte), Integer), CType(CType(64, Byte), Integer))
        Me.TChart1.Legend.Font.Size = 9
        Me.TChart1.Legend.Font.SizeFloat = 9.0!
        '
        '
        '
        Me.TChart1.Legend.Shadow.Visible = False
        Me.TChart1.Location = New System.Drawing.Point(5, 27)
        Me.TChart1.Name = "TChart1"
        '
        '
        '
        '
        '
        '
        Me.TChart1.Panel.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        '
        '
        '
        Me.TChart1.Panel.Brush.Gradient.Visible = False
        Me.TChart1.Series.Add(Me.Line1)
        Me.TChart1.Series.Add(Me.Line2)
        Me.TChart1.Series.Add(Me.Line3)
        Me.TChart1.Series.Add(Me.Line4)
        Me.TChart1.Series.Add(Me.Line5)
        Me.TChart1.Series.Add(Me.Line6)
        Me.TChart1.Series.Add(Me.Line7)
        Me.TChart1.Series.Add(Me.Line8)
        Me.TChart1.Size = New System.Drawing.Size(543, 342)
        Me.TChart1.TabIndex = 1
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TChart1.Walls.Back.Brush.Visible = False
        Me.TChart1.Walls.Back.Visible = False
        '
        'Line1
        '
        '
        '
        '
        Me.Line1.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(119, Byte), Integer), CType(CType(153, Byte), Integer), CType(CType(214, Byte), Integer))
        Me.Line1.Color = System.Drawing.Color.FromArgb(CType(CType(119, Byte), Integer), CType(CType(153, Byte), Integer), CType(CType(214, Byte), Integer))
        Me.Line1.ColorEach = False
        '
        '
        '
        Me.Line1.LinePen.Color = System.Drawing.Color.FromArgb(CType(CType(71, Byte), Integer), CType(CType(92, Byte), Integer), CType(CType(128, Byte), Integer))
        '
        '
        '
        '
        '
        '
        Me.Line1.Marks.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        '
        '
        '
        Me.Line1.Marks.Brush.Gradient.EndColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        Me.Line1.Marks.Brush.Gradient.MiddleColor = System.Drawing.Color.FromArgb(CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer))
        Me.Line1.Marks.Brush.Gradient.StartColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(215, Byte), Integer), CType(CType(0, Byte), Integer))
        Me.Line1.Marks.Brush.Gradient.UseMiddle = True
        '
        '
        '
        '
        '
        '
        Me.Line1.Marks.Font.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(17, Byte), Integer), CType(CType(17, Byte), Integer), CType(CType(17, Byte), Integer))
        '
        '
        '
        Me.Line1.Marks.Pen.Color = System.Drawing.Color.FromArgb(CType(CType(107, Byte), Integer), CType(CType(138, Byte), Integer), CType(CType(193, Byte), Integer))
        '
        '
        '
        Me.Line1.Marks.Shadow.Visible = False
        '
        '
        '
        Me.Line1.Marks.TailParams.Align = Steema.TeeChart.Styles.TailAlignment.[Auto]
        Me.Line1.Marks.TailParams.CustomPointPos = CType(resources.GetObject("resource.CustomPointPos"), System.Drawing.PointF)
        Me.Line1.Marks.TailParams.Margin = 0!
        Me.Line1.Marks.TailParams.PointerHeight = 5.0R
        Me.Line1.Marks.TailParams.PointerWidth = 8.0R
        Me.Line1.Marks.TailStyle = Steema.TeeChart.Styles.MarksTail.WithPointer
        Me.Line1.OriginalCursor = Nothing
        '
        '
        '
        Me.Line1.Pointer.SizeDouble = 0R
        Me.Line1.Pointer.SizeUnits = Steema.TeeChart.Styles.PointerSizeUnits.Pixels
        Me.Line1.Title = "line1"
        Me.Line1.UseExtendedNumRange = False
        '
        '
        '
        Me.Line1.XValues.DataMember = "X"
        Me.Line1.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending
        '
        '
        '
        Me.Line1.YValues.DataMember = "Y"
        '
        'Line2
        '
        '
        '
        '
        Me.Line2.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(207, Byte), Integer), CType(CType(104, Byte), Integer))
        Me.Line2.Color = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(207, Byte), Integer), CType(CType(104, Byte), Integer))
        Me.Line2.ColorEach = False
        '
        '
        '
        Me.Line2.LinePen.Color = System.Drawing.Color.FromArgb(CType(CType(153, Byte), Integer), CType(CType(124, Byte), Integer), CType(CType(62, Byte), Integer))
        '
        '
        '
        '
        '
        '
        Me.Line2.Marks.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        '
        '
        '
        Me.Line2.Marks.Brush.Gradient.EndColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        Me.Line2.Marks.Brush.Gradient.MiddleColor = System.Drawing.Color.FromArgb(CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer))
        Me.Line2.Marks.Brush.Gradient.StartColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(215, Byte), Integer), CType(CType(0, Byte), Integer))
        Me.Line2.Marks.Brush.Gradient.UseMiddle = True
        '
        '
        '
        '
        '
        '
        Me.Line2.Marks.Font.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(17, Byte), Integer), CType(CType(17, Byte), Integer), CType(CType(17, Byte), Integer))
        '
        '
        '
        Me.Line2.Marks.Pen.Color = System.Drawing.Color.FromArgb(CType(CType(230, Byte), Integer), CType(CType(186, Byte), Integer), CType(CType(94, Byte), Integer))
        '
        '
        '
        Me.Line2.Marks.Shadow.Visible = False
        '
        '
        '
        Me.Line2.Marks.TailParams.Align = Steema.TeeChart.Styles.TailAlignment.[Auto]
        Me.Line2.Marks.TailParams.CustomPointPos = CType(resources.GetObject("resource.CustomPointPos1"), System.Drawing.PointF)
        Me.Line2.Marks.TailParams.Margin = 0!
        Me.Line2.Marks.TailParams.PointerHeight = 5.0R
        Me.Line2.Marks.TailParams.PointerWidth = 8.0R
        Me.Line2.Marks.TailStyle = Steema.TeeChart.Styles.MarksTail.WithPointer
        Me.Line2.OriginalCursor = Nothing
        '
        '
        '
        Me.Line2.Pointer.SizeDouble = 0R
        Me.Line2.Pointer.SizeUnits = Steema.TeeChart.Styles.PointerSizeUnits.Pixels
        Me.Line2.Title = "line2"
        Me.Line2.UseExtendedNumRange = False
        '
        '
        '
        Me.Line2.XValues.DataMember = "X"
        Me.Line2.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending
        '
        '
        '
        Me.Line2.YValues.DataMember = "Y"
        '
        'Line3
        '
        '
        '
        '
        Me.Line3.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(127, Byte), Integer), CType(CType(71, Byte), Integer))
        Me.Line3.Color = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(127, Byte), Integer), CType(CType(71, Byte), Integer))
        Me.Line3.ColorEach = False
        '
        '
        '
        Me.Line3.LinePen.Color = System.Drawing.Color.FromArgb(CType(CType(153, Byte), Integer), CType(CType(76, Byte), Integer), CType(CType(43, Byte), Integer))
        '
        '
        '
        '
        '
        '
        Me.Line3.Marks.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        '
        '
        '
        Me.Line3.Marks.Brush.Gradient.EndColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        Me.Line3.Marks.Brush.Gradient.MiddleColor = System.Drawing.Color.FromArgb(CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer))
        Me.Line3.Marks.Brush.Gradient.StartColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(215, Byte), Integer), CType(CType(0, Byte), Integer))
        Me.Line3.Marks.Brush.Gradient.UseMiddle = True
        '
        '
        '
        '
        '
        '
        Me.Line3.Marks.Font.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(17, Byte), Integer), CType(CType(17, Byte), Integer), CType(CType(17, Byte), Integer))
        '
        '
        '
        Me.Line3.Marks.Pen.Color = System.Drawing.Color.FromArgb(CType(CType(230, Byte), Integer), CType(CType(114, Byte), Integer), CType(CType(64, Byte), Integer))
        '
        '
        '
        Me.Line3.Marks.Shadow.Visible = False
        '
        '
        '
        Me.Line3.Marks.TailParams.Align = Steema.TeeChart.Styles.TailAlignment.[Auto]
        Me.Line3.Marks.TailParams.CustomPointPos = CType(resources.GetObject("resource.CustomPointPos2"), System.Drawing.PointF)
        Me.Line3.Marks.TailParams.Margin = 0!
        Me.Line3.Marks.TailParams.PointerHeight = 5.0R
        Me.Line3.Marks.TailParams.PointerWidth = 8.0R
        Me.Line3.Marks.TailStyle = Steema.TeeChart.Styles.MarksTail.WithPointer
        Me.Line3.OriginalCursor = Nothing
        '
        '
        '
        Me.Line3.Pointer.SizeDouble = 0R
        Me.Line3.Pointer.SizeUnits = Steema.TeeChart.Styles.PointerSizeUnits.Pixels
        Me.Line3.Title = "line3"
        Me.Line3.UseExtendedNumRange = False
        '
        '
        '
        Me.Line3.XValues.DataMember = "X"
        Me.Line3.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending
        '
        '
        '
        Me.Line3.YValues.DataMember = "Y"
        '
        'Line4
        '
        '
        '
        '
        Me.Line4.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(129, Byte), Integer), CType(CType(202, Byte), Integer), CType(CType(219, Byte), Integer))
        Me.Line4.Color = System.Drawing.Color.FromArgb(CType(CType(129, Byte), Integer), CType(CType(202, Byte), Integer), CType(CType(219, Byte), Integer))
        Me.Line4.ColorEach = False
        '
        '
        '
        Me.Line4.LinePen.Color = System.Drawing.Color.FromArgb(CType(CType(77, Byte), Integer), CType(CType(121, Byte), Integer), CType(CType(131, Byte), Integer))
        '
        '
        '
        '
        '
        '
        Me.Line4.Marks.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        '
        '
        '
        Me.Line4.Marks.Brush.Gradient.EndColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        Me.Line4.Marks.Brush.Gradient.MiddleColor = System.Drawing.Color.FromArgb(CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer))
        Me.Line4.Marks.Brush.Gradient.StartColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(215, Byte), Integer), CType(CType(0, Byte), Integer))
        Me.Line4.Marks.Brush.Gradient.UseMiddle = True
        '
        '
        '
        '
        '
        '
        Me.Line4.Marks.Font.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(17, Byte), Integer), CType(CType(17, Byte), Integer), CType(CType(17, Byte), Integer))
        '
        '
        '
        Me.Line4.Marks.Pen.Color = System.Drawing.Color.FromArgb(CType(CType(116, Byte), Integer), CType(CType(182, Byte), Integer), CType(CType(197, Byte), Integer))
        '
        '
        '
        Me.Line4.Marks.Shadow.Visible = False
        '
        '
        '
        Me.Line4.Marks.TailParams.Align = Steema.TeeChart.Styles.TailAlignment.[Auto]
        Me.Line4.Marks.TailParams.CustomPointPos = CType(resources.GetObject("resource.CustomPointPos3"), System.Drawing.PointF)
        Me.Line4.Marks.TailParams.Margin = 0!
        Me.Line4.Marks.TailParams.PointerHeight = 5.0R
        Me.Line4.Marks.TailParams.PointerWidth = 8.0R
        Me.Line4.Marks.TailStyle = Steema.TeeChart.Styles.MarksTail.WithPointer
        Me.Line4.OriginalCursor = Nothing
        '
        '
        '
        Me.Line4.Pointer.SizeDouble = 0R
        Me.Line4.Pointer.SizeUnits = Steema.TeeChart.Styles.PointerSizeUnits.Pixels
        Me.Line4.Title = "line4"
        Me.Line4.UseExtendedNumRange = False
        '
        '
        '
        Me.Line4.XValues.DataMember = "X"
        Me.Line4.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending
        '
        '
        '
        Me.Line4.YValues.DataMember = "Y"
        '
        'Line5
        '
        '
        '
        '
        Me.Line5.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(94, Byte), Integer), CType(CType(115, Byte), Integer), CType(CType(158, Byte), Integer))
        Me.Line5.Color = System.Drawing.Color.FromArgb(CType(CType(94, Byte), Integer), CType(CType(115, Byte), Integer), CType(CType(158, Byte), Integer))
        Me.Line5.ColorEach = False
        '
        '
        '
        Me.Line5.LinePen.Color = System.Drawing.Color.FromArgb(CType(CType(56, Byte), Integer), CType(CType(69, Byte), Integer), CType(CType(95, Byte), Integer))
        '
        '
        '
        '
        '
        '
        Me.Line5.Marks.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        '
        '
        '
        Me.Line5.Marks.Brush.Gradient.EndColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        Me.Line5.Marks.Brush.Gradient.MiddleColor = System.Drawing.Color.FromArgb(CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer))
        Me.Line5.Marks.Brush.Gradient.StartColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(215, Byte), Integer), CType(CType(0, Byte), Integer))
        Me.Line5.Marks.Brush.Gradient.UseMiddle = True
        '
        '
        '
        '
        '
        '
        Me.Line5.Marks.Font.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(17, Byte), Integer), CType(CType(17, Byte), Integer), CType(CType(17, Byte), Integer))
        '
        '
        '
        Me.Line5.Marks.Pen.Color = System.Drawing.Color.FromArgb(CType(CType(85, Byte), Integer), CType(CType(104, Byte), Integer), CType(CType(142, Byte), Integer))
        '
        '
        '
        Me.Line5.Marks.Shadow.Visible = False
        '
        '
        '
        Me.Line5.Marks.TailParams.Align = Steema.TeeChart.Styles.TailAlignment.[Auto]
        Me.Line5.Marks.TailParams.CustomPointPos = CType(resources.GetObject("resource.CustomPointPos4"), System.Drawing.PointF)
        Me.Line5.Marks.TailParams.Margin = 0!
        Me.Line5.Marks.TailParams.PointerHeight = 5.0R
        Me.Line5.Marks.TailParams.PointerWidth = 8.0R
        Me.Line5.Marks.TailStyle = Steema.TeeChart.Styles.MarksTail.WithPointer
        Me.Line5.OriginalCursor = Nothing
        '
        '
        '
        Me.Line5.Pointer.SizeDouble = 0R
        Me.Line5.Pointer.SizeUnits = Steema.TeeChart.Styles.PointerSizeUnits.Pixels
        Me.Line5.Title = "line5"
        Me.Line5.UseExtendedNumRange = False
        '
        '
        '
        Me.Line5.XValues.DataMember = "X"
        Me.Line5.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending
        '
        '
        '
        Me.Line5.YValues.DataMember = "Y"
        '
        'Line6
        '
        '
        '
        '
        Me.Line6.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(80, Byte), Integer), CType(CType(174, Byte), Integer), CType(CType(150, Byte), Integer))
        Me.Line6.Color = System.Drawing.Color.FromArgb(CType(CType(80, Byte), Integer), CType(CType(174, Byte), Integer), CType(CType(150, Byte), Integer))
        Me.Line6.ColorEach = False
        '
        '
        '
        Me.Line6.LinePen.Color = System.Drawing.Color.FromArgb(CType(CType(48, Byte), Integer), CType(CType(104, Byte), Integer), CType(CType(90, Byte), Integer))
        '
        '
        '
        '
        '
        '
        Me.Line6.Marks.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        '
        '
        '
        Me.Line6.Marks.Brush.Gradient.EndColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        Me.Line6.Marks.Brush.Gradient.MiddleColor = System.Drawing.Color.FromArgb(CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer))
        Me.Line6.Marks.Brush.Gradient.StartColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(215, Byte), Integer), CType(CType(0, Byte), Integer))
        Me.Line6.Marks.Brush.Gradient.UseMiddle = True
        '
        '
        '
        '
        '
        '
        Me.Line6.Marks.Font.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(17, Byte), Integer), CType(CType(17, Byte), Integer), CType(CType(17, Byte), Integer))
        '
        '
        '
        Me.Line6.Marks.Pen.Color = System.Drawing.Color.FromArgb(CType(CType(72, Byte), Integer), CType(CType(157, Byte), Integer), CType(CType(135, Byte), Integer))
        '
        '
        '
        Me.Line6.Marks.Shadow.Visible = False
        '
        '
        '
        Me.Line6.Marks.TailParams.Align = Steema.TeeChart.Styles.TailAlignment.[Auto]
        Me.Line6.Marks.TailParams.CustomPointPos = CType(resources.GetObject("resource.CustomPointPos5"), System.Drawing.PointF)
        Me.Line6.Marks.TailParams.Margin = 0!
        Me.Line6.Marks.TailParams.PointerHeight = 5.0R
        Me.Line6.Marks.TailParams.PointerWidth = 8.0R
        Me.Line6.Marks.TailStyle = Steema.TeeChart.Styles.MarksTail.WithPointer
        Me.Line6.OriginalCursor = Nothing
        '
        '
        '
        Me.Line6.Pointer.SizeDouble = 0R
        Me.Line6.Pointer.SizeUnits = Steema.TeeChart.Styles.PointerSizeUnits.Pixels
        Me.Line6.Title = "line6"
        Me.Line6.UseExtendedNumRange = False
        '
        '
        '
        Me.Line6.XValues.DataMember = "X"
        Me.Line6.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending
        '
        '
        '
        Me.Line6.YValues.DataMember = "Y"
        '
        'Line7
        '
        '
        '
        '
        Me.Line7.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(230, Byte), Integer), CType(CType(59, Byte), Integer), CType(CType(65, Byte), Integer))
        Me.Line7.Color = System.Drawing.Color.FromArgb(CType(CType(230, Byte), Integer), CType(CType(59, Byte), Integer), CType(CType(65, Byte), Integer))
        Me.Line7.ColorEach = False
        '
        '
        '
        Me.Line7.LinePen.Color = System.Drawing.Color.FromArgb(CType(CType(138, Byte), Integer), CType(CType(35, Byte), Integer), CType(CType(39, Byte), Integer))
        '
        '
        '
        '
        '
        '
        Me.Line7.Marks.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        '
        '
        '
        Me.Line7.Marks.Brush.Gradient.EndColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        Me.Line7.Marks.Brush.Gradient.MiddleColor = System.Drawing.Color.FromArgb(CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer))
        Me.Line7.Marks.Brush.Gradient.StartColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(215, Byte), Integer), CType(CType(0, Byte), Integer))
        Me.Line7.Marks.Brush.Gradient.UseMiddle = True
        '
        '
        '
        '
        '
        '
        Me.Line7.Marks.Font.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(17, Byte), Integer), CType(CType(17, Byte), Integer), CType(CType(17, Byte), Integer))
        '
        '
        '
        Me.Line7.Marks.Pen.Color = System.Drawing.Color.FromArgb(CType(CType(207, Byte), Integer), CType(CType(53, Byte), Integer), CType(CType(58, Byte), Integer))
        '
        '
        '
        Me.Line7.Marks.Shadow.Visible = False
        '
        '
        '
        Me.Line7.Marks.TailParams.Align = Steema.TeeChart.Styles.TailAlignment.[Auto]
        Me.Line7.Marks.TailParams.CustomPointPos = CType(resources.GetObject("resource.CustomPointPos6"), System.Drawing.PointF)
        Me.Line7.Marks.TailParams.Margin = 0!
        Me.Line7.Marks.TailParams.PointerHeight = 5.0R
        Me.Line7.Marks.TailParams.PointerWidth = 8.0R
        Me.Line7.Marks.TailStyle = Steema.TeeChart.Styles.MarksTail.WithPointer
        Me.Line7.OriginalCursor = Nothing
        '
        '
        '
        Me.Line7.Pointer.SizeDouble = 0R
        Me.Line7.Pointer.SizeUnits = Steema.TeeChart.Styles.PointerSizeUnits.Pixels
        Me.Line7.Title = "line7"
        Me.Line7.UseExtendedNumRange = False
        '
        '
        '
        Me.Line7.XValues.DataMember = "X"
        Me.Line7.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending
        '
        '
        '
        Me.Line7.YValues.DataMember = "Y"
        '
        'Line8
        '
        '
        '
        '
        Me.Line8.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(0, Byte), Integer), CType(CType(0, Byte), Integer))
        Me.Line8.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(0, Byte), Integer), CType(CType(0, Byte), Integer))
        Me.Line8.ColorEach = False
        '
        '
        '
        Me.Line8.LinePen.Color = System.Drawing.Color.FromArgb(CType(CType(153, Byte), Integer), CType(CType(146, Byte), Integer), CType(CType(86, Byte), Integer))
        '
        '
        '
        '
        '
        '
        Me.Line8.Marks.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        '
        '
        '
        Me.Line8.Marks.Brush.Gradient.EndColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        Me.Line8.Marks.Brush.Gradient.MiddleColor = System.Drawing.Color.FromArgb(CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer), CType(CType(128, Byte), Integer))
        Me.Line8.Marks.Brush.Gradient.StartColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(215, Byte), Integer), CType(CType(0, Byte), Integer))
        Me.Line8.Marks.Brush.Gradient.UseMiddle = True
        '
        '
        '
        '
        '
        '
        Me.Line8.Marks.Font.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(17, Byte), Integer), CType(CType(17, Byte), Integer), CType(CType(17, Byte), Integer))
        '
        '
        '
        Me.Line8.Marks.Shadow.Visible = False
        '
        '
        '
        Me.Line8.Marks.TailParams.Align = Steema.TeeChart.Styles.TailAlignment.[Auto]
        Me.Line8.Marks.TailParams.CustomPointPos = CType(resources.GetObject("resource.CustomPointPos7"), System.Drawing.PointF)
        Me.Line8.Marks.TailParams.Margin = 0!
        Me.Line8.Marks.TailParams.PointerHeight = 5.0R
        Me.Line8.Marks.TailParams.PointerWidth = 8.0R
        Me.Line8.Marks.TailStyle = Steema.TeeChart.Styles.MarksTail.WithPointer
        Me.Line8.OriginalCursor = Nothing
        '
        '
        '
        Me.Line8.Pointer.HorizSize = 3
        Me.Line8.Pointer.SizeDouble = 0R
        Me.Line8.Pointer.SizeUnits = Steema.TeeChart.Styles.PointerSizeUnits.Pixels
        Me.Line8.Pointer.VertSize = 3
        Me.Line8.Pointer.Visible = True
        Me.Line8.Title = "line8"
        Me.Line8.UseExtendedNumRange = False
        '
        '
        '
        Me.Line8.XValues.DataMember = "X"
        Me.Line8.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending
        '
        '
        '
        Me.Line8.YValues.DataMember = "Y"
        '
        'ChartController1
        '
        Me.ChartController1.ButtonSize = Steema.TeeChart.ControllerButtonSize.x16
        Me.ChartController1.Chart = Me.TChart1
        Me.ChartController1.LabelValues = True
        Me.ChartController1.Location = New System.Drawing.Point(0, 0)
        Me.ChartController1.Name = "ChartController1"
        Me.ChartController1.Size = New System.Drawing.Size(548, 25)
        Me.ChartController1.TabIndex = 2
        Me.ChartController1.Text = "ChartController1"
        '
        'grfGas
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.ChartController1)
        Me.Controls.Add(Me.TChart1)
        Me.Name = "grfGas"
        Me.Size = New System.Drawing.Size(548, 369)
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents TChart1 As Steema.TeeChart.TChart
    Friend WithEvents Line1 As Steema.TeeChart.Styles.Line
    Friend WithEvents Line2 As Steema.TeeChart.Styles.Line
    Friend WithEvents Line3 As Steema.TeeChart.Styles.Line
    Friend WithEvents Line4 As Steema.TeeChart.Styles.Line
    Friend WithEvents Line5 As Steema.TeeChart.Styles.Line
    Friend WithEvents Line6 As Steema.TeeChart.Styles.Line
    Friend WithEvents Line7 As Steema.TeeChart.Styles.Line
    Friend WithEvents Line8 As Steema.TeeChart.Styles.Line
    Friend WithEvents ChartController1 As Steema.TeeChart.ChartController
End Class
