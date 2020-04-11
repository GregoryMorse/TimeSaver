Imports System.Numerics
Imports WpfMath
Imports WpfMath.Boxes
Imports WpfMath.Rendering
Imports WpfMath.Rendering.Transformations
Imports System.Windows

Public Class frmMain
    Private Sub tmrMain_Tick(ByVal sender As Object, ByVal e As System.EventArgs) Handles tmrMain.Tick
        Invalidate()
    End Sub
    Private Sub frmMain_Deactivate(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Deactivate
        Me.Activate()
    End Sub
    Private Sub frmMain_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        'Dim Count As Integer
        'Dim Solution As ValueTuple(Of EquationFinder.Solution, EquationFinder.Solution)
        'Dim Result As String = ""
        'Dim res As Array() = EquationFinder.Subsets(New Object() {1, 2, 3, 4})
        'res = EquationFinder.GetPartitions(4)
        'res = EquationFinder.GetContigParts(1234)
        'For Count = 0 To 100
        '    EquationFinder.SplitSolve(4444) ', Count)
        '    Solution = EquationFinder.Solve(New BigInteger() {4, 4, 4, 4}, New BigInteger() {Count})
        '    Result += Solution.Item1.GetEquationString() + "=" + Solution.Item2.GetEquationString() + vbCrLf
        'Next
        'MessageBox.Show(Result)
    End Sub
    'Private Class MyElementRenderer
    '    Implements Rendering.IElementRenderer

    '    Public Sub RenderElement(box As Box, x As Double, y As Double) Implements IElementRenderer.RenderElement
    '        Throw New NotImplementedException()
    '    End Sub

    '    Public Sub RenderGlyphRun(scaledGlyphFactory As Func(Of Double, Windows.Media.GlyphRun), x As Double, y As Double, foreground As Windows.Media.Brush) Implements IElementRenderer.RenderGlyphRun
    '        Throw New NotImplementedException()
    '    End Sub

    '    Public Sub RenderRectangle(rectangle As Windows.Rect, foreground As Windows.Media.Brush) Implements IElementRenderer.RenderRectangle
    '        Throw New NotImplementedException()
    '    End Sub

    '    Public Sub RenderTransformed(box As Box, transforms() As Transformation, x As Double, y As Double) Implements IElementRenderer.RenderTransformed
    '        Throw New NotImplementedException()
    '    End Sub

    '    Public Sub FinishRendering() Implements IElementRenderer.FinishRendering
    '        Throw New NotImplementedException()
    '    End Sub
    'End Class
    Private Sub TexToPng(ByVal Str As String, ByVal g As Graphics)
        Dim Parser As New TexFormulaParser()
        Dim Formula As TexFormula = Parser.Parse("\color {blue} {" + Str + "}")
        'Dim Renderer As TexRenderer = Formula.GetRenderer(TexStyle.Display, 20.0, "Arial")
        'Renderer.RenderFormulaTo(New MyElementRenderer, 0.0, 0.0)
        Dim bytes As Byte() = Formula.RenderToPng(100.0, 0.0, 0.0, "Arial")
        Dim MemStream As New System.IO.MemoryStream(bytes)
        Dim img As Image = Image.FromStream(MemStream)
        g.DrawImage(img, New PointF((Me.ClientSize.Width - img.Width) / 2, 16))
        img.Dispose()
        MemStream.Dispose()
    End Sub
    Private Sub frmMain_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles Me.Paint
        Dim target As New Rectangle(48, Me.ClientSize.Height / 4, Me.ClientSize.Width - 96, Me.ClientSize.Height / 2)
        Dim the_font As New Font("Microsoft Sans Serif", target.Height, System.Drawing.FontStyle.Bold, GraphicsUnit.Pixel)
        Dim sf As New StringFormat
        sf.Alignment = StringAlignment.Center
        sf.LineAlignment = StringAlignment.Center
        Dim text_path As New Drawing2D.GraphicsPath
        Dim CurDate As Date = TimeOfDay
        text_path.AddString(CurDate, the_font.FontFamily, CInt(System.Drawing.FontStyle.Bold), target.Height, New PointF(0, 0), sf)
        Dim text_rectf As RectangleF = text_path.GetBounds()
        Dim target_pts() As PointF = {
            New PointF(target.Left, target.Top),
            New PointF(target.Right, target.Top),
            New PointF(target.Left, target.Bottom)
        }
        e.Graphics.Transform = New Drawing2D.Matrix(text_rectf, target_pts)
        e.Graphics.Clear(Me.BackColor)
        e.Graphics.SmoothingMode = Drawing2D.SmoothingMode.AntiAlias
        e.Graphics.FillPath(Brushes.Red, text_path)
        e.Graphics.DrawPath(Pens.Red, text_path)
        e.Graphics.ResetTransform()
        Dim Sol As ValueTuple(Of EquationFinder.Solution, EquationFinder.Solution) = EquationFinder.SplitSolve(TimeOfDay.Hour * 10000 + TimeOfDay.Minute * 100 + TimeOfDay.Second)
        TexToPng(Sol.Item1.GetLatexEquationString() + "=" + Sol.Item2.GetLatexEquationString(), e.Graphics)
        text_path.Dispose()
        sf.Dispose()
        the_font.Dispose()
    End Sub
End Class