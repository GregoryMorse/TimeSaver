Imports System.Runtime.InteropServices
Imports System.Numerics

'famous test cases include the four fours problem https://en.wikipedia.org/wiki/Four_fours
'http://www.murderousmaths.co.uk/books/4x4ans.htm
'https://www.cut-the-knot.org/arithmetic/funny/4_4.shtml
'could calibrate advanced math fo example Euler's number exponent using e, i, pi, 1, 0 for e^(i*pi)+1=0
'numberphile posted about a paper dealing with numbers 0 or 1 through 9 to make any number - must find and check with this!
'there is a classic challenge about generating any number using pi which was solved with infinite logs and square roots - a general method
'four pi's challenge: https://www.youtube.com/watch?v=tXunUuvx6_c +/-ln(ln pi/ln sqrt(...[n]sqrt(pi)))/ln(-cos pi-cos pi)=n
'single pi: -log(sqrt((-cos pi)% %...%))=n
'https://www.cut-the-knot.org/arithmetic/funny/Dirac.shtml#solution

Public Class EquationFinder
    Shared Function SubsetsK(ByVal List As Object(), ByVal Min As Integer, ByVal k As Integer) As Array
        Dim Output As New ArrayList
        If List.Length = 1 Then
            Output.Add(New Array() {List})
            Return Output.ToArray(GetType(Array()))
        End If
        Dim first As Object = List(0)
        Dim Parts As Array() = SubsetsK(List.Skip(1).ToArray(), Min - 1, k)
        For i = 0 To Parts.Length - 1
            Dim smaller As Array() = Parts(i)
            If smaller.Length > k Then Continue For
            'insert `first` In Each Of the subpartition's subsets
            If smaller.Length >= Min Then
                For n = 0 To smaller.Length - 1
                    Output.Add(smaller.Take(n).Append(New Object() {first}.Concat(smaller(n)).ToArray()).Concat(smaller.Skip(n + 1)).ToArray())
                Next
            End If
            'put `first` In its own subset 
            If smaller.Length < k Then Output.Add(New Array() {New Object() {first}}.Concat(smaller).ToArray())
        Next
        Return Output.ToArray(GetType(Array()))
    End Function
    Shared Function Subsets(ByVal List As Object()) As Array
        Dim Output As New ArrayList
        If List.Length = 1 Then
            Output.Add(New Array() {List})
            Return Output.ToArray(GetType(Array()))
        End If
        Dim first As Object = List(0)
        Dim Parts As Array() = Subsets(List.Skip(1).ToArray())
        For i = 0 To Parts.Length - 1
            Dim smaller As Array() = Parts(i)
            'insert `first` In Each Of the subpartition's subsets
            For n = 0 To smaller.Length - 1
                If n = 1 Then Exit For 'For contiguous only
                Output.Add(smaller.Take(n).Append(New Object() {first}.Concat(smaller(n)).ToArray()).Concat(smaller.Skip(n + 1)).ToArray())
            Next
            'put `first` In its own subset 
            Output.Add(New Array() {New Object() {first}}.Concat(smaller).ToArray())
        Next
        Return Output.ToArray(GetType(Array()))
    End Function
    Shared Sub PartitionHelper(ByVal List As Integer(), ByVal Index As Integer, ByRef Result As ArrayList)
        If Index = List.Length - 1 Then
            Result.Add(List)
            Return
        End If
        'While List(Index) >= List(Index + 1) 'unique partition permutations
        While List(Index) <> 0 'all partition permutations
            Dim NewList(List.Length - 1) As Integer
            List.CopyTo(NewList, 0)
            PartitionHelper(NewList, Index + 1, Result)
            List(Index) -= 1
            List(Index + 1) += 1
        End While
    End Sub
    Shared Function GetPartitions(ByVal Number As Integer) As Array()
        'Get partitions of all lengths 1 to Number
        Dim Count As Integer
        Dim Result As New ArrayList
        For Count = 1 To Number
            Dim List(Count - 1) As Integer
            List(0) = Number - (Count - 1)
            For i = 1 To List.Length - 1
                List(i) = 1
            Next
            PartitionHelper(List, 0, Result)
        Next
        Return Result.ToArray(GetType(Array))
    End Function
    Shared Function Factorial(ByVal Number As BigInteger) As BigInteger
        Dim Result As BigInteger = BigInteger.One
        While Number <> 0
            Result = Result * Number
            Number = Number - 1
        End While
        Return Result
        'If Number = 0 Then Return 1 Else Return Number * Factorial(Number - 1)
    End Function
    Public Shared Function GetContigParts(ByVal Number As BigInteger)
        'Generate all integer partitions summing to the number of digits
        'First generate the partitions and then permute through them
        Dim Parts As New ArrayList
        Dim Count As Integer
        Dim NumSize As Integer = Math.Ceiling(Math.Log10(Number))
        Dim Partitions As Array() = GetPartitions(NumSize)
        For Count = 0 To Partitions.Length - 1
            Dim Numbers(Partitions(Count).Length - 1) As BigInteger
            Dim CurNum As Integer = Number
            Dim RemNumSize As Integer = NumSize
            For NumCount = 0 To Partitions(Count).Length - 1
                RemNumSize = RemNumSize - Partitions(Count)(NumCount)
                Numbers(NumCount) = CurNum \ Math.Pow(10, RemNumSize)
                CurNum = CurNum Mod Math.Pow(10, RemNumSize)
            Next
            Parts.Add(Numbers)
        Next
        Return Parts.ToArray(GetType(BigInteger()))
    End Function
    Public Shared Function SplitSolve(ByVal Number As BigInteger) As Object()
        Dim Parts As Array() = GetContigParts(Number)
        For i = 0 To Parts.Length - 1
            Dim Solution As Object() = Solve(Parts(i))
            If Not Solution Is Nothing Then Return Solution
        Next
        Return Nothing
    End Function
    MustInherit Class Solution
        Implements IComparable(Of Solution)
        Public Solution As BigInteger
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
        Public Sub New(ByVal _Solution As BigInteger, ByVal _Initial As Solution, ByVal _Operation As eUnaryOperation)
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
                    Return "." + CStr(Initial.GetEquationString()) + "̅" '... trailing notation is better and more common
                Case eUnaryOperation.eNone
                    Return Solution.ToString()
                Case Else
                    Return Solution.ToString()
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
        Public Sub New(ByVal _Solution As BigInteger, ByVal _Left As Solution, ByVal _Right As Solution, ByVal _Operation As eBinaryOperation)
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
    Const UnaryOperationDepth = 2
    Shared MaxSubSolution As BigInteger = BigInteger.One << 64
    Shared UnaryDict As New Dictionary(Of BigInteger, UnarySolution())
    Shared BinaryDict As New Dictionary(Of ValueTuple(Of BigInteger, BigInteger), BinarySolution())
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
    Shared Function IntegerLogarithm10(ByVal Number As BigInteger) As BigInteger
        'Fast algorithm assuming can determine initial n: Number mod 10^n + Number / 10^n then use n/2
        'maximum is floor(base*log 2) so choose nearest power of 2
        Dim Digits As BigInteger = NumberOfDigits(Number)
        Return IIf(IntegerPositivePower(10, Digits - 1) = Number, Digits - 1, BigInteger.MinusOne)
    End Function
    Shared Function CheckMul(ByVal Number1 As BigInteger, ByVal Number2 As BigInteger) As Boolean
        Return True
        'If Number1 = -1 Then Return Number2 <> Int64.MinValue
        'If Number1 = 0 Then Return True
        'Division rounding down must also use remainder?
        'Dim Remainder As BigInteger = Nothing
        'Dim Result As BigInteger = BigInteger.DivRem(IIf(Number1 < 0, Int64.MinValue, Int64.MaxValue), Number1, Remainder)
        'Return Result >= BigInteger.Abs(Number2)
    End Function
    Shared Function IntegerPositivePower(ByVal Number As BigInteger, ByVal Exponent As BigInteger) As BigInteger
        Return BigInteger.Pow(Number, Exponent)
        'IntegerPositivePower = 1
        'While Exponent <> 0
        ' If (Exponent Mod 2) = 1 Then
        'If CheckMul(IntegerPositivePower, Number) Then
        'IntegerPositivePower *= Number
        'Else
        'Return -1
        'End If
        'End If
        'Exponent /= 2
        'If CheckMul(Number, Number) Then
        'Number *= Number
        'Else
        'Return -1
        'End If
        'End While
    End Function
    Shared Function GetBitSize(Number)
        Dim bytes() As Byte = Number.ToByteArray()
        Dim Size As Integer = bytes.Length
        If Size = 0 Then Return 0
        Dim v As Integer = bytes(Size - 1) '8-bit value to find the log2 of 
        If v = 0 Then Return (Size - 1) * 8
        Dim r As Integer 'result of log2(v) will go here
        Dim shift As Integer
        r = If(v > &HF, 4, 0)
        v >>= r
        shift = If(v > 3, 2, 0)
        v >>= shift
        r = r Or shift
        r = r Or (v >> 1)
        Return (Size - 1) * 8 + r + 1
    End Function
    Shared Function isSqrt(ByVal n As BigInteger, ByVal root As BigInteger)
        Dim lowerBound As BigInteger = root * root
        Return n >= lowerBound AndAlso n <= lowerBound + root + root
    End Function
    Shared Function IntegerSquareRoot(ByVal Number As BigInteger) As BigInteger
        If Number = 0 Then Return BigInteger.Zero
        If Number < 0 Then Return BigInteger.MinusOne
        Dim bitLength As Integer = GetBitSize(Number)
        Dim root As BigInteger = BigInteger.One << (bitLength >> 1)
        While Not isSqrt(Number, root)
            root += Number / root
            root >>= 1
        End While
        Return root
        'Dim Root As BigInteger = 0
        'Dim RTry As BigInteger
        'square root begins with log n?
        'Dim Count As BigInteger = 31
        'While True
        '    RTry = Root + (1UL << Count)
        '    If Number >= (RTry << Count) Then
        '        Number -= (RTry << Count)
        '        Root = Root Or (2UL << Count)
        '    End If
        '    If Count = 0 Then Exit While
        '    Count = Count - 1
        'End While
        'Return Root >> 1
    End Function
    Shared Function NumberOfDigits(ByVal Number As BigInteger) As Long
        Dim Log10 As Integer = BigInteger.Log10(Number)
        Return Log10 + 1 'If(IntegerPositivePower(10, Log10) = Number, Log10, Log10 + 1)
        'Dim Digits As Int64 = 0
        'Dim NextComp As Int64 = 16 'maximum is ceiling(base*log 2) so choose nearest power of 2
        'While NextComp <> 0
        '    If IntegerPositivePower(10, Digits + NextComp) = -1 Or
        '        IntegerPositivePower(10, Digits + NextComp) >= BigInteger.Abs(Number) Then
        '        Digits += NextComp
        '    End If
        '    NextComp >>= 1
        'End While
        'Return Digits + 1
    End Function
    Shared Function GetUnarySolution(ByVal Number As Solution) As Solution()
        If UnaryDict.ContainsKey(Number.Solution) Then Return UnaryDict(Number.Solution).Select(Function(sol) New UnarySolution(sol.Solution, Number, sol.Operation)).ToArray()
        Dim Solution As New List(Of UnarySolution)
        Dim Result As BigInteger
        'Must detect overflows without using very slow exception handling code
        If Number.Solution < 0 Then
            'eliminate even roots of negative numbers which are imaginery numbers
            'If Number.Solution = Int64.MinValue Then
            Solution.Add(New UnarySolution(-Number.Solution, Number, UnarySolution.eUnaryOperation.eNegation))
            'End If
            'Solution.Add(New UnarySolution(Math.Exp(Number.Solution), Number, UnarySolution.eUnaryOperation.eNaturalExponent))
            'Solution.Add(New UnarySolution(IntegerPositivePower(10, Number.Solution), Number, UnarySolution.eUnaryOperation.ePowerOf10))
            If Number.Solution Mod 100 = 0 Then
                Solution.Add(New UnarySolution(Number.Solution / 100, Number, UnarySolution.eUnaryOperation.ePercent))
            End If
            'Solution.Add(New UnarySolution(Number.Solution / IntegerPositivePower(10, NumberOfDigits(-Number.Solution)), Number, UnarySolution.eUnaryOperation.eDecimal))
            'Solution.Add(New UnarySolution(Number.Solution / (IntegerPositivePower(10, NumberOfDigits(-Number.Solution)) - 1), Number, UnarySolution.eUnaryOperation.eDecimal))
        ElseIf Number.Solution = 0 Then
            Solution.Add(New UnarySolution(Factorial(Number.Solution), Number, UnarySolution.eUnaryOperation.eFactorial))
            'Solution.Add(New UnarySolution(IntegerPositivePower(10, Number.Solution), Number, UnarySolution.eUnaryOperation.ePowerOf10))
        Else
            Solution.Add(New UnarySolution(-Number.Solution, Number, UnarySolution.eUnaryOperation.eNegation))
            Result = IntegerSquareRoot(Number.Solution)
            If Result * Result = Number.Solution Then
                Solution.Add(New UnarySolution(Result, Number, UnarySolution.eUnaryOperation.eSquareRoot))
            End If
            If (Number.Solution <= 16) Then Solution.Add(New UnarySolution(Factorial(Number.Solution), Number, UnarySolution.eUnaryOperation.eFactorial))
            'Solution.Add(New UnarySolution(Math.Exp(Number.Solution), Number, UnarySolution.eUnaryOperation.eNaturalExponent))
            'Solution.Add(New UnarySolution(Math.Log(Number.Solution), Number, UnarySolution.eUnaryOperation.eNaturalLogarithm))
            If IntegerLogarithm10(Number.Solution) <> -1 Then
                Solution.Add(New UnarySolution(IntegerLogarithm10(Number.Solution), Number, UnarySolution.eUnaryOperation.eLogarithmBase10))
            End If
            If Number.Solution <= 18 Then 'AndAlso IntegerPositivePower(10, Number.Solution) <> -1 Then
                Solution.Add(New UnarySolution(IntegerPositivePower(10, Number.Solution), Number, UnarySolution.eUnaryOperation.ePowerOf10))
            End If
            If (Number.Solution - 1 <= 16) Then Solution.Add(New UnarySolution(Factorial(Number.Solution - 1), Number, UnarySolution.eUnaryOperation.eGamma))
            If Number.Solution Mod 100 = 0 Then
                Solution.Add(New UnarySolution(Number.Solution / 100, Number, UnarySolution.eUnaryOperation.ePercent))
            End If
        End If
        Dim Possible As New HashSet(Of BigInteger)
        Dim i As Integer = 0
        While i <> Solution.Count
            If Solution(i).Solution > MaxSubSolution OrElse Possible.Contains(Solution(i).Solution) Then
                Solution.RemoveAt(i)
            Else
                Possible.Add(Solution(i).Solution)
                i += 1
            End If
        End While
        UnaryDict.Add(Number.Solution, Solution.ToArray())
        Return UnaryDict(Number.Solution)
    End Function
    Shared Function GetBinarySolution(ByVal Number1 As Solution, ByVal Number2 As Solution) As Solution()
        If BinaryDict.ContainsKey((Number1.Solution, Number2.Solution)) Then Return BinaryDict((Number1.Solution, Number2.Solution)).Select(Function(sol) New BinarySolution(sol.Solution, Number1, Number2, sol.Operation)).ToArray()
        Dim Solution As New List(Of BinarySolution)
        Solution.Add(New BinarySolution(Number1.Solution + Number2.Solution, Number1, Number2, BinarySolution.eBinaryOperation.eAddition))
        Solution.Add(New BinarySolution(Number1.Solution - Number2.Solution, Number1, Number2, BinarySolution.eBinaryOperation.eSubtraction))
        'If CheckMul(Number1.Solution, Number2.Solution) Then
        Solution.Add(New BinarySolution(Number1.Solution * Number2.Solution, Number1, Number2, BinarySolution.eBinaryOperation.eMultiplication))
        'End If
        If Number2.Solution <> 0 AndAlso Number1.Solution Mod Number2.Solution = 0 Then 'Number2.Solution * (Number1.Solution / Number2.Solution) = Number1.Solution Then
            Solution.Add(New BinarySolution(Number1.Solution / Number2.Solution, Number1, Number2, BinarySolution.eBinaryOperation.eDivision))
        End If
        If Number2.Solution <> 0 Then
            Solution.Add(New BinarySolution(Number1.Solution Mod Number2.Solution, Number1, Number2, BinarySolution.eBinaryOperation.eModulus))
        End If
        'Prevent imaginary numbers such as negative numbers to binary reciprical powers
        If Number2.Solution >= 0 AndAlso Number2.Solution <= 256 Then ' AndAlso IntegerPositivePower(Number1.Solution, Number2.Solution) <> -1 Then
            Solution.Add(New BinarySolution(IntegerPositivePower(Number1.Solution, Number2.Solution), Number1, Number2, BinarySolution.eBinaryOperation.ePower))
            'Solution.Add(New BinarySolution(Number1.Solution ^ Number2.Solution, Number1, Number2, BinarySolution.eBinaryOperation.ePower))
        End If
        BinaryDict.Add((Number1.Solution, Number2.Solution), Solution.ToArray())
        Return BinaryDict((Number1.Solution, Number2.Solution))
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
        Return Enumerable.GroupBy(CType(Solutions.ToArray(GetType(Solution)), Solution()), Function(KeySelector As Solution) KeySelector.Solution).Select(Function(Selector As IGrouping(Of BigInteger, Solution)) Selector.OrderBy(Function(KeySelector As Solution) KeySelector.CountUnaryOperations()).First()).ToArray()
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
    Public Shared Function Solve(ByVal LeftNumbers As BigInteger(), ByVal RightNumbers As BigInteger()) As Object()
        'Accept Solutions from set on left and right with least unary operations
        Dim Solutions As Object() = GetSolutions(LeftNumbers).GroupJoin(
                            GetSolutions(RightNumbers),
                            Function(Key As Solution) Key.Solution,
                            Function(Key As Solution) Key.Solution,
                            Function(Match As Solution, Results As IEnumerable(Of Solution)) IIf(Results.Count = 0, New Object() {Nothing}, New Object() {Match, Results.DefaultIfEmpty(Nothing).FirstOrDefault(), LeftNumbers.Length})).Where(Function(Sol As Object()) Sol.Length <> 1).ToArray()
        If Solutions.Count = 0 Then Return New Object() {New UnarySolution(0, Nothing, UnarySolution.eUnaryOperation.eNone), New UnarySolution(0, Nothing, UnarySolution.eUnaryOperation.eNone), 0}
        Return Solutions.OrderBy(Function(Sol As Object()) Sol(0).CountUnaryOperations() + Sol(1).CountUnaryOperations()).First()
    End Function
    Public Shared Function Solve(ByVal Numbers As BigInteger()) As Object()
        Dim Count As Integer
        Dim Solutions As Object() 'BigInteger(), BigInteger(), = position
        'Place equivalence operator first
        For Count = 1 To Numbers.Length - 1
            'Left Solutions
            Dim LeftNumbers(Count - 1) As BigInteger
            Array.ConstrainedCopy(Numbers, 0, LeftNumbers, 0, Count)
            'Right Solutions
            Dim RightNumbers(Numbers.Length - Count - 1) As BigInteger
            Array.ConstrainedCopy(Numbers, Count, RightNumbers, 0, Numbers.Length - Count)
            'Accept Solutions from set on left and right with least unary operations
            Solutions = Solve(LeftNumbers, RightNumbers)
            If Not Solutions Is Nothing Then Return Solutions
        Next
        Return Nothing
    End Function
End Class