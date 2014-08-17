Imports System.Runtime.InteropServices

Public Class EquationFinder
    Shared Sub PartitionHelper(ByVal List As Array, ByVal Index As Integer, ByRef Result As ArrayList)
        If Index = List.Length - 1 Then Return
        While List(Index) - 1 >= List(Index + 1) + 1
            List(Index) -= 1
            List(Index + 1) += 1
            Result.Add(List)
            PartitionHelper(List, Index + 1, Result)
        End While
    End Sub
    Shared Function GetPartitions(ByVal Number As Integer) As Array()
        'Get partitions of all lengths 1 to Number
        Dim Count As Integer
        Dim List() As Integer
        Dim Result As New ArrayList
        For Count = 1 To Number
            ReDim List(Count)
            List(0) = Number
            PartitionHelper(List, 0, Result)
        Next
        Return Result.ToArray(GetType(Array))
    End Function
    Shared Function Factorial(ByVal Number As Integer) As Integer
        If Number = 0 Then Return 1 Else Return Number * Factorial(Number - 1)
    End Function
    Shared Function GetPermutations(ByVal Partition() As Integer) As Array()
        Dim Result(Factorial(Partition.Length - 1)) As Array
        'Eliminate duplicates for speed
        Dim Count As Integer
        Dim SubCount As Integer
        Dim RowCount As Integer
        Dim NewPartition() As Integer
        Dim TempInt As Integer
        Result(0) = Partition
        For Count = 1 To Partition.Length - 2
            For SubCount = 0 To RowCount
                For NewCount = Partition.Length To Count + 2 Step -1
                    TempInt = Result(SubCount)(Count)
                    NewPartition = Result(SubCount)
                    NewPartition(Count + 1) = NewPartition(NewCount - 1)
                    NewPartition(NewCount) = TempInt
                    RowCount = RowCount + 1
                    Result(RowCount) = NewPartition
                Next
            Next
        Next
        Return Result
    End Function
    Public Shared Function SplitSolve(ByVal Number As Integer) As Object()
        'Generate all integer partitions summing to the number of digits
        'First generate the partitions and then permute through them
        Dim Count As Integer
        Dim SubCount As Integer
        Dim Partitions As Array() = GetPartitions(Math.Ceiling(Math.Log10(Number)))
        Dim Solution As Object()
        For Count = 0 To Partitions.Length - 1
            Dim Permutations As Array() = GetPermutations(Partitions(Count))
            For SubCount = 0 To Permutations.Length - 1
                Dim Numbers(Permutations(SubCount).Length) As Int64
                Dim CurNum As Integer = Number
                For NumCount = 0 To Permutations(SubCount).Length - 1
                    Numbers(NumCount) = CurNum \ Math.Pow(10, Math.Ceiling(Math.Log10(Number)) - Permutations(SubCount)(NumCount))
                    CurNum = CurNum Mod Math.Pow(10, Math.Ceiling(Math.Log10(Number)) - Permutations(SubCount)(NumCount))
                Next
                Solution = Solve(Numbers)
                If Not Solution Is Nothing Then Return Solution
            Next
        Next
        Return Nothing
    End Function
    MustInherit Class Solution
        Implements IComparable(Of Solution)
        Public Solution As Int64
        Public MustOverride Function CountUnaryOperations() As Integer
        Public MustOverride Function GetEquationString() As String
        Public Overloads Function CompareTo(ByVal other As Solution) As Integer Implements IComparable(Of Solution).CompareTo
            Dim UnOpLeft As Integer = CountUnaryOperations()
            Dim UnOpRight As Integer = other.CountUnaryOperations()
            If UnOpLeft = UnOpRight Then Return 0
            If UnOpLeft > UnOpRight Then Return 1 Else Return -1
        End Function
    End Class
    Class UnarySolution
        Inherits Solution
        Public Initial As Solution
        Public Operation As eUnaryOperation
        Enum eUnaryOperation
            eNone
            eNegation
            eNaturalExponent
            eNaturalLogarithm
            ePowerOf10
            eLogarithmBase10
            eSquareRoot
            eFactorial
            eGamma
            ePercent
            eDecimal
            eDecimalRepeating
        End Enum
        Public Sub New(ByVal _Solution As Int64, ByVal _Initial As Solution, ByVal _Operation As eUnaryOperation)
            Solution = _Solution
            Initial = _Initial
            Operation = _Operation
        End Sub
        Public Overrides Function CountUnaryOperations() As Integer
            If Operation = eUnaryOperation.eNone Then Return 0 Else Return 1 + Initial.CountUnaryOperations()
        End Function
        Public Overrides Function GetEquationString() As String
            Select Case Operation
                Case eUnaryOperation.eNegation
                    Return "-" + CStr(Initial.GetEquationString())
                Case eUnaryOperation.eNaturalExponent
                    Return "e^" + CStr(Initial.GetEquationString())
                Case eUnaryOperation.eNaturalLogarithm
                    Return "ln " + CStr(Initial.GetEquationString())
                Case eUnaryOperation.ePowerOf10
                    Return "E" + CStr(Initial.GetEquationString()) '"10^"
                Case eUnaryOperation.eLogarithmBase10
                    Return "log " + CStr(Initial.GetEquationString())
                Case eUnaryOperation.eSquareRoot
                    Return "√" + CStr(Initial.GetEquationString()) '"sqrt"
                Case eUnaryOperation.eFactorial
                    Return CStr(Initial.GetEquationString()) + "!"
                Case eUnaryOperation.eGamma
                    Return "Γ" + CStr(Initial.GetEquationString())
                Case eUnaryOperation.ePercent
                    Return CStr(Initial.GetEquationString()) + "%"
                Case eUnaryOperation.eDecimal
                    Return "." + CStr(Initial.GetEquationString())
                Case eUnaryOperation.eDecimalRepeating
                    Return "." + CStr(Initial.GetEquationString()) + "̅"
                Case eUnaryOperation.eNone
                    Return CStr(Solution)
                Case Else
                    Return CStr(Solution)
            End Select
        End Function
    End Class
    Class BinarySolution
        Inherits Solution
        Public Left As Solution
        Public Right As Solution
        Public Operation As eBinaryOperation
        Enum eBinaryOperation
            eAddition
            eSubtraction
            eMultiplication
            eDivision
            eModulus
            ePower
        End Enum
        Public Sub New(ByVal _Solution As Int64, ByVal _Left As Solution, ByVal _Right As Solution, ByVal _Operation As eBinaryOperation)
            Solution = _Solution
            Left = _Left
            Right = _Right
            Operation = _Operation
        End Sub
        Public Overrides Function CountUnaryOperations() As Integer
            Return Left.CountUnaryOperations() + Right.CountUnaryOperations()
        End Function
        Public Overrides Function GetEquationString() As String
            'Need parenthesis
            Select Case Operation
                Case eBinaryOperation.eAddition
                    Return "(" + Left.GetEquationString() + "+" + Right.GetEquationString() + ")"
                Case eBinaryOperation.eSubtraction
                    Return "(" + Left.GetEquationString() + "-" + Right.GetEquationString() + ")"
                Case eBinaryOperation.eMultiplication
                    'Add parenthesis if inner operation precedence lower
                    Return "(" + Left.GetEquationString() + "*" + Right.GetEquationString() + ")"
                Case eBinaryOperation.eDivision
                    'Add parenthesis if inner operation precedence lower
                    Return "(" + Left.GetEquationString() + "/" + Right.GetEquationString() + ")"
                Case eBinaryOperation.eModulus
                    'Add parenthesis if inner operation precedence lower
                    Return "(" + Left.GetEquationString() + " Mod " + Right.GetEquationString() + ")"
                Case eBinaryOperation.ePower
                    'Add parenthesis if inner operation precedence lower
                    Return "(" + Left.GetEquationString() + "^" + Right.GetEquationString() + ")"
                Case Else
                    Return ""
            End Select
        End Function
    End Class
    'Unary operation depth configurable
    Const UnaryOperationDepth = 3
    Shared Function GetUnarySolutions(ByVal Number As Solution) As Solution()
        Dim Solutions As New ArrayList
        Dim Count As Integer
        Dim SubCount As Integer
        Dim StartCount As Integer = 0
        Dim EndCount As Integer
        Solutions.Add(Number)
        For Count = 0 To UnaryOperationDepth - 2
            EndCount = Solutions.Count
            For SubCount = StartCount To EndCount - 1
                Solutions.AddRange(GetUnarySolution(Solutions(SubCount)))
            Next
            StartCount = EndCount
        Next
        Return Solutions.ToArray(GetType(Solution))
    End Function
    Shared Function IntegerLogarithm10(ByVal Number As Int64) As Int64
        'Fast algorithm assuming can determine initial n: Number mod 10^n + Number / 10^n then use n/2
        'maximum is floor(base*log 2) so choose nearest power of 2
        Dim Digits As Int64 = NumberOfDigits(Number)
        Return IIf(IntegerPositivePower(10, Digits - 1) = Number, Digits, -1)
    End Function
    Shared Function CheckMul(ByVal Number1 As Int64, ByVal Number2 As Int64) As Boolean
        If Number1 = -1 Then Return Number2 <> Int64.MinValue
        If Number1 = 0 Then Return True
        'Division rounding down must also use remainder?
        Dim Remainder As Int64
        Dim Result As Int64 = Math.DivRem(IIf(Number1 < 0, Int64.MinValue, Int64.MaxValue), Number1, Remainder)
        Return Result >= Math.Abs(Number2)
    End Function
    Shared Function IntegerPositivePower(ByVal Number As Int64, ByVal Exponent As Int64) As Int64
        IntegerPositivePower = 1
        While Exponent <> 0
            If (Exponent Mod 2) = 1 Then
                If CheckMul(IntegerPositivePower, Number) Then
                    IntegerPositivePower *= Number
                Else
                    Return -1
                End If
            End If
            Exponent \= 2
            If CheckMul(Number, Number) Then
                Number *= Number
            Else
                Return -1
            End If
        End While
    End Function
    Shared Function IntegerSquareRoot(ByVal Number As UInt64) As Int64
        Dim Root As UInt64 = 0
        Dim RTry As UInt64
        'square root begins with log n?
        Dim Count As UInt64 = 31
        While True
            RTry = Root + (1UL << Count)
            If Number >= (RTry << Count) Then
                Number -= (RTry << Count)
                Root = Root Or (2UL << Count)
            End If
            If Count = 0 Then Exit While
            Count = Count - 1
        End While
        Return Root >> 1
    End Function
    Shared Function NumberOfDigits(ByVal Number As Int64) As Int64
        Dim Digits As Int64 = 0
        Dim NextComp As Int64 = 16 'maximum is ceiling(base*log 2) so choose nearest power of 2
        While NextComp <> 0
            If IntegerPositivePower(10, Digits + NextComp) = -1 Or _
                IntegerPositivePower(10, Digits + NextComp) >= Math.Abs(Number) Then
                Digits += NextComp
            End If
            NextComp >>= 1
        End While
        Return Digits + 1
    End Function
    Shared Function GetUnarySolution(ByVal Number As Solution) As Solution()
        Dim Solution As New ArrayList
        Dim Result As Int64
        'Must detect overflows without using very slow exception handling code
        If Number.Solution < 0 Then
            'eliminate even roots of negative numbers which are imaginery numbers
            If Number.Solution <> Int64.MinValue Then
                Solution.Add(New UnarySolution(-Number.Solution, Number, UnarySolution.eUnaryOperation.eNegation))
            End If
            'Solution.Add(New UnarySolution(Math.Exp(Number.Solution), Number, UnarySolution.eUnaryOperation.eNaturalExponent))
            'Solution.Add(New UnarySolution(IntegerPositivePower(10, Number.Solution), Number, UnarySolution.eUnaryOperation.ePowerOf10))
            Solution.Add(New UnarySolution(Number.Solution \ 100, Number, UnarySolution.eUnaryOperation.ePercent))
            Solution.Add(New UnarySolution(Number.Solution \ IntegerPositivePower(10, NumberOfDigits(Number.Solution)), Number, UnarySolution.eUnaryOperation.eDecimal))
            Solution.Add(New UnarySolution(Number.Solution \ (IntegerPositivePower(10, NumberOfDigits(Number.Solution)) - 1), Number, UnarySolution.eUnaryOperation.eDecimal))
        ElseIf Number.Solution = 0 Then
            Solution.Add(New UnarySolution(IntegerPositivePower(10, Number.Solution), Number, UnarySolution.eUnaryOperation.ePowerOf10))
            Solution.Add(New UnarySolution(Factorial(Number.Solution), Number, UnarySolution.eUnaryOperation.eFactorial))
        Else
            Solution.Add(New UnarySolution(-Number.Solution, Number, UnarySolution.eUnaryOperation.eNegation))
            'Solution.Add(New UnarySolution(Math.Exp(Number.Solution), Number, UnarySolution.eUnaryOperation.eNaturalExponent))
            'Solution.Add(New UnarySolution(Math.Log(Number.Solution), Number, UnarySolution.eUnaryOperation.eNaturalLogarithm))
            If Number.Solution <= 18 AndAlso IntegerPositivePower(10, Number.Solution) <> -1 Then
                Solution.Add(New UnarySolution(IntegerPositivePower(10, Number.Solution), Number, UnarySolution.eUnaryOperation.ePowerOf10))
            End If
            If IntegerLogarithm10(Number.Solution) <> -1 Then
                Solution.Add(New UnarySolution(IntegerLogarithm10(Number.Solution), Number, UnarySolution.eUnaryOperation.eLogarithmBase10))
            End If
            Result = IntegerSquareRoot(Number.Solution)
            If Result * Result = Number.Solution Then
                Solution.Add(New UnarySolution(Result, Number, UnarySolution.eUnaryOperation.eSquareRoot))
            End If
            Solution.Add(New UnarySolution(Factorial(Number.Solution), Number, UnarySolution.eUnaryOperation.eFactorial))
            Solution.Add(New UnarySolution(Factorial(Number.Solution - 1), Number, UnarySolution.eUnaryOperation.eGamma))
            Solution.Add(New UnarySolution(Number.Solution \ 100, Number, UnarySolution.eUnaryOperation.ePercent))
        End If
        Return Solution.ToArray(GetType(Solution))
    End Function
    Shared Function GetBinarySolution(ByVal Number1 As Solution, ByVal Number2 As Solution) As Solution()
        Dim Solution As New ArrayList
        Solution.Add(New BinarySolution(Number1.Solution + Number2.Solution, Number1, Number2, BinarySolution.eBinaryOperation.eAddition))
        Solution.Add(New BinarySolution(Number1.Solution - Number2.Solution, Number1, Number2, BinarySolution.eBinaryOperation.eSubtraction))
        If CheckMul(Number1.Solution, Number2.Solution) Then
            Solution.Add(New BinarySolution(Number1.Solution * Number2.Solution, Number1, Number2, BinarySolution.eBinaryOperation.eMultiplication))
        End If
        If Number2.Solution <> 0 AndAlso Number2.Solution * (Number1.Solution \ Number2.Solution) = Number1.Solution Then
            Solution.Add(New BinarySolution(Number1.Solution \ Number2.Solution, Number1, Number2, BinarySolution.eBinaryOperation.eDivision))
        End If
        If Number2.Solution <> 0 Then
            Solution.Add(New BinarySolution(Number1.Solution Mod Number2.Solution, Number1, Number2, BinarySolution.eBinaryOperation.eModulus))
        End If
        'Prevent imaginary numbers such as negative numbers to binary reciprical powers
        If Number2.Solution >= 0 AndAlso IntegerPositivePower(Number1.Solution, Number2.Solution) <> -1 Then
            Solution.Add(New BinarySolution(IntegerPositivePower(Number1.Solution, Number2.Solution), Number1, Number2, BinarySolution.eBinaryOperation.ePower))
        Else
            'Solution.Add(New BinarySolution(Number1.Solution ^ Number2.Solution, Number1, Number2, BinarySolution.eBinaryOperation.ePower))
        End If
        Return Solution.ToArray(GetType(Solution))
    End Function
    Shared Function GetCombinedSolutions(ByVal Numbers As Array()) As Solution()
        Dim Count As Integer
        Dim LeftCount As Integer
        Dim RightCount As Integer
        Dim Solutions As New ArrayList
        If Numbers.Length = 1 Then Solutions.AddRange(Numbers(0))
        For Count = 1 To Numbers.Length - 1
            Dim LeftSolutions As New ArrayList
            Dim RightSolutions As New ArrayList
            Dim LeftNumbers(Count - 1) As Array
            Array.ConstrainedCopy(Numbers, 0, LeftNumbers, 0, Count)
            LeftSolutions.AddRange(GetCombinedSolutions(LeftNumbers))
            Dim RightNumbers(Numbers.Length - Count - 1) As Array
            Array.ConstrainedCopy(Numbers, Count, RightNumbers, 0, Numbers.Length - Count)
            RightSolutions.AddRange(GetCombinedSolutions(RightNumbers))
            For LeftCount = 0 To LeftSolutions.Count - 1
                For RightCount = 0 To RightSolutions.Count - 1
                    For Each Element In GetBinarySolution(LeftSolutions(LeftCount), RightSolutions(RightCount))
                        Solutions.AddRange(GetUnarySolutions(Element))
                    Next
                Next
            Next
        Next
        Return Enumerable.GroupBy(CType(Solutions.ToArray(GetType(Solution)), Solution()), Function(KeySelector As Solution) KeySelector.Solution).Select(Function(Selector As IGrouping(Of Long, Solution)) Selector.OrderBy(Function(KeySelector As Solution) KeySelector.CountUnaryOperations()).First()).ToArray()
    End Function
    '2 or more numbers
    Shared Function GetSolutions(ByVal Numbers As Array) As Solution()
        Dim Solutions As New ArrayList
        For Count = 0 To Numbers.Length - 1
            'Unary solutions
            Solutions.Add(GetUnarySolutions(New UnarySolution(Numbers(Count), Nothing, UnarySolution.eUnaryOperation.eNone)))
        Next
        'Binary combinations done both directions to simulate parenthesis
        Return GetCombinedSolutions(Solutions.ToArray(GetType(Array)))
    End Function
    Public Shared Function Solve(ByVal LeftNumbers As Int64(), ByVal RightNumbers As Int64()) As Object()
        'Accept Solutions from set on left and right with least unary operations
        Dim Solutions As Object() = GetSolutions(LeftNumbers).GroupJoin( _
                            GetSolutions(RightNumbers), _
                            Function(Key As Solution) Key.Solution, _
                            Function(Key As Solution) Key.Solution, _
                            Function(Match As Solution, Results As IEnumerable(Of Solution)) IIf(Results.Count = 0, New Object() {Nothing}, New Object() {Match, Results.DefaultIfEmpty(Nothing).FirstOrDefault(), LeftNumbers.Length})).Where(Function(Sol As Object()) Sol.Length <> 1).ToArray()
        If Solutions.Count = 0 Then Return New Object() {New UnarySolution(0, Nothing, UnarySolution.eUnaryOperation.eNone), New UnarySolution(0, Nothing, UnarySolution.eUnaryOperation.eNone), 0}
        Return Solutions.OrderBy(Function(Sol As Object()) Sol(0).CountUnaryOperations() + Sol(1).CountUnaryOperations()).First()
    End Function
    Public Shared Function Solve(ByVal Numbers As Int64()) As Object()
        Dim Count As Integer
        Dim Solutions As Object() 'Int64(), Int64(), = position
        'Place equivalence operator first
        For Count = 1 To Numbers.Length - 1
            'Left Solutions
            Dim LeftNumbers(Count - 1) As Int64
            Array.ConstrainedCopy(Numbers, 0, LeftNumbers, 0, Count)
            'Right Solutions
            Dim RightNumbers(Numbers.Length - Count - 1) As Int64
            Array.ConstrainedCopy(Numbers, Count, RightNumbers, 0, Numbers.Length - Count)
            'Accept Solutions from set on left and right with least unary operations
            Solutions = Solve(LeftNumbers, RightNumbers)
            If Not Solutions Is Nothing Then Return Solutions
        Next
        Return Nothing
    End Function
End Class