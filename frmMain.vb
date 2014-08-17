Public Class frmMain
    Private Sub tmrMain_Tick(ByVal sender As Object, ByVal e As System.EventArgs) Handles tmrMain.Tick
        Invalidate()
    End Sub
    Private Sub frmMain_Deactivate(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Deactivate
        Me.Activate()
    End Sub
    Private Sub frmMain_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        'Dim Count As Integer
        'Dim Solution As Object()
        'Dim Result As String = ""
        'For Count = 0 To 100
        '    'EquationFinder.SplitSolve(4444, Count)
        '    Solution = EquationFinder.Solve(New Int64() {4, 4, 4, 4}, New Int64() {Count})
        '    Result += Solution(0).GetEquationString() + "=" + Solution(1).GetEquationString() + vbCrLf
        'Next
        'MessageBox.Show(Result)
    End Sub
    Private Sub frmMain_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles Me.Paint
        Dim target As New Rectangle(48, Me.ClientSize.Height / 4, Me.ClientSize.Width - 96, Me.ClientSize.Height / 2)
        Dim the_font As New Font("Microsoft Sans Serif", target.Height, FontStyle.Bold, GraphicsUnit.Pixel)
        Dim sf As New StringFormat
        sf.Alignment = StringAlignment.Center
        sf.LineAlignment = StringAlignment.Center
        Dim text_path As New Drawing2D.GraphicsPath
        text_path.AddString(TimeOfDay, the_font.FontFamily, CInt(FontStyle.Bold), target.Height, New PointF(0, 0), sf)
        Dim text_rectf As RectangleF = text_path.GetBounds()
        Dim target_pts() As PointF = { _
            New PointF(target.Left, target.Top), _
            New PointF(target.Right, target.Top), _
            New PointF(target.Left, target.Bottom) _
        }
        e.Graphics.Transform = New Drawing2D.Matrix(text_rectf, target_pts)
        e.Graphics.Clear(Me.BackColor)
        e.Graphics.SmoothingMode = Drawing2D.SmoothingMode.AntiAlias
        e.Graphics.FillPath(Brushes.Red, text_path)
        e.Graphics.DrawPath(Pens.Red, text_path)
        e.Graphics.ResetTransform()
        text_path.Dispose()
        sf.Dispose()
        the_font.Dispose()
    End Sub
End Class