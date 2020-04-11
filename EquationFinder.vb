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

Public Class Irrational
    'non integral: pi, e^, log10, ln, log(base), i, fractional powers - sqrt, nth root, cos, sin, tan, cosh, sinh, tanh
    'overly large: E/10^, n^, a*b, a/b where b<1, !
End Class
Public Class Rational
    Public Numerator As BigInteger
    Public Denominator As BigInteger
    Public Sub New(ByVal _Numerator As BigInteger, ByVal _Denominator As BigInteger)
        Dim gcd As BigInteger = BigInteger.GreatestCommonDivisor(Numerator, Denominator)
        If gcd = BigInteger.One Then
            Numerator = _Numerator
            Denominator = _Denominator
        Else
            Numerator = _Numerator / gcd
            Denominator = _Denominator / gcd
        End If
    End Sub
    Public Shared Function SimplifyRational(ByVal r As Rational)
        Dim gcd As BigInteger = BigInteger.GreatestCommonDivisor(r.Numerator, r.Denominator)
        If gcd = BigInteger.One Then Return r
        Return New Rational(r.Numerator / gcd, r.Denominator / gcd)
    End Function
    Public Shared Function Divide(dividend As Rational, divisor As Rational) As Rational
        Return dividend / divisor
    End Function
    Public Shared Function Multiply(left As Rational, right As Rational) As Rational
        Return left * right
    End Function
    Public Shared Function Subtract(left As Rational, right As Rational) As Rational
        Return left - right
    End Function
    Public Shared Function Add(left As Rational, right As Rational) As Rational
        Return left + right
    End Function
    Public Shared Function Abs(value As Rational) As Rational
        If value.Numerator.Sign < 1 Then
            Return New Rational(-value.Numerator, value.Denominator)
        ElseIf value.Denominator.Sign < 1 Then
            Return New Rational(value.Numerator, -value.Denominator)
        End If
        Return value
    End Function
    Public Shared Operator +(left As Rational, right As Rational) As Rational
        Dim gcd As BigInteger = BigInteger.GreatestCommonDivisor(left.Denominator, right.Denominator) 'lcm is left.Denominator * right.Denominator / gcd
        Return SimplifyRational(New Rational(left.Numerator * right.Denominator / gcd + right.Numerator * left.Denominator / gcd, left.Denominator * right.Denominator / gcd))
    End Operator
    Public Shared Operator -(left As Rational, right As Rational) As Rational
        Dim gcd As BigInteger = BigInteger.GreatestCommonDivisor(left.Denominator, right.Denominator) 'lcm is left.Denominator * right.Denominator / gcd
        Return SimplifyRational(New Rational(left.Numerator * right.Denominator / gcd - right.Numerator * left.Denominator / gcd, left.Denominator * right.Denominator / gcd))
    End Operator
    Public Shared Operator *(left As Rational, right As Rational) As Rational
        Return SimplifyRational(New Rational(left.Numerator * right.Numerator, left.Denominator * right.Denominator))
    End Operator
    Public Shared Operator /(dividend As Rational, divisor As Rational) As Rational
        Return SimplifyRational(New Rational(dividend.Numerator * divisor.Denominator, dividend.Denominator * divisor.Numerator))
    End Operator
End Class

Public Class EquationFinder
    Shared Function SubsetsK(ByVal List As Object(), ByVal Min As Integer, ByVal k As Integer) As Array
        Dim Output As New List(Of Array)
        If List.Length = 1 Then
            Output.Add(New Array() {List})
            Return Output.ToArray()
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
        Return Output.ToArray()
    End Function
    Shared Function Subsets(ByVal List As Object()) As Array
        Dim Output As New List(Of Array)
        If List.Length = 1 Then
            Output.Add(New Array() {List})
            Return Output.ToArray()
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
        Return Output.ToArray()
    End Function
    Shared Sub PartitionHelper(ByVal List As Integer(), ByVal Index As Integer, ByRef Result As List(Of Array))
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
        Dim Result As New List(Of Array)
        For Count = 1 To Number
            Dim List(Count - 1) As Integer
            List(0) = Number - (Count - 1)
            For i = 1 To List.Length - 1
                List(i) = 1
            Next
            PartitionHelper(List, 0, Result)
        Next
        Return Result.ToArray()
    End Function
    Shared Function Factorial(ByVal Number As BigInteger) As BigInteger
        If Factorials.Count = 0 Then
            Factorials.Add(0, 1)
            Factorials.Add(1, 1)
            Dim Result As BigInteger = BigInteger.One
            Dim i As BigInteger = 2
            While Result <= MaxSubSolution
                Result *= i
                Factorials.Add(i, Result)
                i += 1
            End While
        End If
        If Factorials.ContainsKey(Number) Then
            Return Factorials(Number)
        Else
            Return -1
        End If
        'If Number = 0 Then Return 1 Else Return Number * Factorial(Number - 1)
    End Function
    Public Shared Function GetContigParts(ByVal Number As BigInteger)
        'Generate all integer partitions summing to the number of digits
        'First generate the partitions and then permute through them
        Dim Parts As New List(Of BigInteger())
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
        Return Parts.ToArray()
    End Function
    Public Shared Function SplitSolve(ByVal Number As BigInteger) As ValueTuple(Of Solution, Solution)
        Dim Parts As Array() = GetContigParts(Number)
        For i = 0 To Parts.Length - 1
            Dim Solution As ValueTuple(Of Solution, Solution) = Solve(Parts(i))
            If Not Solution.Item1 Is Nothing AndAlso Not Solution.Item2 Is Nothing Then Return Solution
        Next
        Return Nothing
    End Function
    MustInherit Class Solution
        Implements IComparable(Of Solution)
        'Left-To-Right Evaluation except for serial exponentiation so operator precedence:
        'Parenthesis - top precedence
        'Unary Operation - always need parenthesis
        'Exponentiation and roots
        'Multiplication/Division/Modulo
        'Addition/Subtraction
        'No precedence for left-sides and initial
        'Additional parenthesis are typically added due to similarity when subtracting a negative, or double negation occur which should not happen due to minimizing unary operations
        Public Enum Precedence
            Unary = 0
            ExpRoots = 1
            MultDivMod = 2
            NoPrec = 3 'AddSub = 3
        End Enum
        'Associativity holds for addition, multiplication allowing removal of parenthesis
        'it does not hold for subtraction, division, modulo or exponentiation (which is usually RTL associative)
        'https://en.wikipedia.org/wiki/Order_of_operations
        Public Solution As BigInteger
        Public MustOverride Function CountUnaryOperations() As Integer
        Public MustOverride Function GetEquationString(Optional ByVal prec As Precedence = Precedence.NoPrec, Optional ByVal IsRight As Boolean = False) As String
        Public MustOverride Function GetLatexEquationString(Optional ByVal prec As Precedence = Precedence.NoPrec, Optional ByVal IsRight As Boolean = False) As String
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
        Public Overrides Function GetEquationString(Optional ByVal prec As Precedence = Precedence.NoPrec, Optional ByVal IsRight As Boolean = False) As String
            Select Case Operation
                Case eUnaryOperation.eNegation
                    Return "-" + CStr(Initial.GetEquationString(Precedence.Unary))
                Case eUnaryOperation.eNaturalExponent
                    Return "e^" + CStr(Initial.GetEquationString(Precedence.Unary))
                Case eUnaryOperation.eNaturalLogarithm
                    Return "ln " + CStr(Initial.GetEquationString(Precedence.Unary))
                Case eUnaryOperation.ePowerOf10
                    Return "ᴇ" + CStr(Initial.GetEquationString(Precedence.Unary)) '"10^" 'E
                Case eUnaryOperation.eLogarithmBase10
                    Return "log " + CStr(Initial.GetEquationString(Precedence.Unary))
                Case eUnaryOperation.eSquareRoot
                    Return "√" + CStr(Initial.GetEquationString(Precedence.Unary)) '"sqrt"
                Case eUnaryOperation.eFactorial
                    Return CStr(Initial.GetEquationString(Precedence.Unary)) + "!"
                Case eUnaryOperation.eGamma
                    Return "Γ" + CStr(Initial.GetEquationString(Precedence.Unary))
                Case eUnaryOperation.ePercent
                    Return CStr(Initial.GetEquationString(Precedence.Unary)) + "%"
                Case eUnaryOperation.eDecimal
                    Return "." + CStr(Initial.GetEquationString(Precedence.Unary))
                Case eUnaryOperation.eDecimalRepeating
                    Return "." + CStr(Initial.GetEquationString(Precedence.Unary)) + "̅" '... trailing notation is better and more common
                Case eUnaryOperation.eNone
                    Return Solution.ToString()
                Case Else
                    Return Solution.ToString()
            End Select
        End Function
        Public Overrides Function GetLatexEquationString(Optional ByVal prec As Precedence = Precedence.NoPrec, Optional ByVal IsRight As Boolean = False) As String
            Select Case Operation
                Case eUnaryOperation.eNegation
                    Return "-" + CStr(Initial.GetLatexEquationString(Precedence.Unary))
                Case eUnaryOperation.eNaturalExponent
                    Return "\exp^{" + CStr(Initial.GetLatexEquationString(Precedence.Unary)) + "}"
                Case eUnaryOperation.eNaturalLogarithm
                    Return "\ln " + CStr(Initial.GetLatexEquationString(Precedence.Unary))
                Case eUnaryOperation.ePowerOf10
                    Return "\text{ᴇ}" + CStr(Initial.GetLatexEquationString(Precedence.Unary))
                Case eUnaryOperation.eLogarithmBase10
                    Return "\log " + CStr(Initial.GetLatexEquationString(Precedence.Unary))
                Case eUnaryOperation.eSquareRoot
                    Return "\sqrt{" + CStr(Initial.GetLatexEquationString(Precedence.Unary)) + "}"
                Case eUnaryOperation.eFactorial
                    Return CStr(Initial.GetLatexEquationString(Precedence.Unary)) + "!"
                Case eUnaryOperation.eGamma
                    Return "\Gamma" + CStr(Initial.GetLatexEquationString(Precedence.Unary))
                Case eUnaryOperation.ePercent
                    Return CStr(Initial.GetLatexEquationString(Precedence.Unary)) + "\%"
                Case eUnaryOperation.eDecimal
                    Return "." + CStr(Initial.GetLatexEquationString(Precedence.Unary))
                Case eUnaryOperation.eDecimalRepeating
                    Return ".\overline{" + CStr(Initial.GetLatexEquationString(Precedence.Unary)) + "}"
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
            eNthRoot
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
        Public Overrides Function GetEquationString(Optional ByVal prec As Precedence = Precedence.NoPrec, Optional ByVal IsRight As Boolean = False) As String
            'Need parenthesis
            Select Case Operation
                Case eBinaryOperation.eAddition
                    Dim S As String = Left.GetLatexEquationString() + "+" + Right.GetLatexEquationString()
                    Return If(prec < Precedence.NoPrec OrElse IsRight, "(" + S + ")", S)
                Case eBinaryOperation.eSubtraction
                    Dim S As String = Left.GetLatexEquationString() + "-" + Right.GetLatexEquationString(Precedence.NoPrec, True)
                    Return If(prec < Precedence.NoPrec OrElse IsRight, "(" + S + ")", S)
                Case eBinaryOperation.eMultiplication
                    'Add parenthesis if inner operation precedence lower
                    Dim S As String = Left.GetEquationString(Precedence.MultDivMod) + "×" + Right.GetEquationString(Precedence.MultDivMod) '*
                    Return If(prec < Precedence.MultDivMod OrElse prec = Precedence.MultDivMod AndAlso IsRight, "(" + S + ")", S)
                Case eBinaryOperation.eDivision
                    'Add parenthesis if inner operation precedence lower
                    Dim S As String = Left.GetEquationString(Precedence.MultDivMod) + "÷" + Right.GetEquationString(Precedence.MultDivMod, True)
                    Return If(prec < Precedence.MultDivMod OrElse prec = Precedence.MultDivMod AndAlso IsRight, "(" + S + ")", S) '/
                Case eBinaryOperation.eModulus
                    'Add parenthesis if inner operation precedence lower
                    Dim S As String = Left.GetEquationString(Precedence.MultDivMod) + " Mod " + Right.GetEquationString(Precedence.MultDivMod, True)
                    Return If(prec < Precedence.MultDivMod OrElse prec = Precedence.MultDivMod AndAlso IsRight, "(" + S + ")", S) '%
                Case eBinaryOperation.ePower
                    'Add parenthesis if inner operation precedence lower - right to left
                    Dim S As String = Left.GetEquationString(Precedence.ExpRoots, True) + "^" + Right.GetEquationString(Precedence.ExpRoots)
                    Return If(prec = Precedence.ExpRoots AndAlso IsRight, "(" + S + ")", S)
                Case eBinaryOperation.eNthRoot
                    Return "(" + Left.GetEquationString(Precedence.MultDivMod) + "√" + Right.GetEquationString(Precedence.MultDivMod, True) + ")"
                Case Else
                    Return ""
            End Select
        End Function
        Public Overrides Function GetLatexEquationString(Optional ByVal prec As Precedence = Precedence.NoPrec, Optional ByVal IsRight As Boolean = False) As String
            'Need parenthesis
            Select Case Operation
                Case eBinaryOperation.eAddition
                    Dim S As String = Left.GetLatexEquationString() + "+" + Right.GetLatexEquationString()
                    Return If(prec < Precedence.NoPrec OrElse IsRight, "(" + S + ")", S)
                Case eBinaryOperation.eSubtraction
                    Dim S As String = Left.GetLatexEquationString() + "-" + Right.GetLatexEquationString(Precedence.NoPrec, True)
                    Return If(prec < Precedence.NoPrec OrElse IsRight, "(" + S + ")", S)
                Case eBinaryOperation.eMultiplication
                    'Add parenthesis if inner operation precedence lower
                    Dim S As String = Left.GetLatexEquationString(Precedence.MultDivMod) + " \times " + Right.GetLatexEquationString(Precedence.MultDivMod)
                    Return If(prec < Precedence.MultDivMod OrElse prec = Precedence.MultDivMod AndAlso IsRight, "(" + S + ")", S)
                Case eBinaryOperation.eDivision
                    'Add parenthesis if inner operation precedence lower
                    Return "\frac{" + Left.GetLatexEquationString() + "}{" + Right.GetLatexEquationString() + "}"
                Case eBinaryOperation.eModulus
                    'Add parenthesis if inner operation precedence lower
                    Dim S As String = Left.GetLatexEquationString(Precedence.MultDivMod) + " \medspace\mathrm{ mod }\medspace " + Right.GetLatexEquationString(Precedence.MultDivMod, True)
                    Return If(prec < Precedence.MultDivMod OrElse prec = Precedence.MultDivMod AndAlso IsRight, "(" + S + ")", S)
                Case eBinaryOperation.ePower
                    'Add parenthesis if outer operation precedence lower - right to left
                    Dim S As String = Left.GetLatexEquationString(Precedence.ExpRoots, True) + "^{" + Right.GetLatexEquationString() + "}"
                    Return If(prec = Precedence.ExpRoots AndAlso IsRight, "(" + S + ")", S)
                Case eBinaryOperation.eNthRoot
                    Return "\sqrt[" + Left.GetLatexEquationString() + "]{" + Right.GetLatexEquationString() + "}"
                Case Else
                    Return ""
            End Select
        End Function
    End Class
    'Unary operation depth configurable
    Const UnaryOperationDepth = 2
    Shared MaxSubSolution As BigInteger = BigInteger.One << 64
    Shared MaxSubSolBits As Integer = -1
    Shared MaxSubSolBase10 As Integer = -1
    Shared Factorials As New Dictionary(Of BigInteger, BigInteger)
    Shared UnaryDict As New Dictionary(Of BigInteger, UnarySolution())
    Shared BinaryDict As New Dictionary(Of ValueTuple(Of BigInteger, BigInteger), BinarySolution())
    Shared Function GetUnarySolutions(ByVal Number As Solution) As Solution()
        Dim Solutions As New List(Of Solution)
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
        Return Solutions.ToArray()
    End Function
    Shared Function IntegerLogarithm10(ByVal Number As BigInteger) As BigInteger
        'Fast algorithm assuming can determine initial n: Number mod 10^n + Number / 10^n then use n/2
        'maximum is floor(base*log 2) so choose nearest power of 2
        Dim Digits As BigInteger = BigInteger.Log10(Number)
        Return IIf(IntegerPositivePower(10, Digits) = Number, Digits, BigInteger.MinusOne)
    End Function
    Shared Function CheckMul(ByVal Number1 As BigInteger, ByVal Number2 As BigInteger) As Boolean
        If MaxSubSolBits = -1 Then MaxSubSolBits = GetBitSize(MaxSubSolution)
        Return GetBitSize(Number1) + GetBitSize(Number2) <= MaxSubSolBits
        'If Number1 = -1 Then Return Number2 <> Int64.MinValue
        'If Number1 = 0 Then Return True
        'Division rounding down must also use remainder?
        'Dim Remainder As BigInteger = Nothing
        'Dim Result As BigInteger = BigInteger.DivRem(IIf(Number1 < 0, Int64.MinValue, Int64.MaxValue), Number1, Remainder)
        'Return Result >= BigInteger.Abs(Number2)
    End Function
    Shared Function IntegerPositivePower(ByVal Number As BigInteger, ByVal Exponent As BigInteger) As BigInteger
        'Return BigInteger.Pow(Number, Exponent)
        Dim Result As BigInteger = BigInteger.One
        While Exponent <> 0
            If (Exponent And BigInteger.One) <> 0 Then
                If CheckMul(Result, Number) Then
                    Result *= Number
                Else
                    Return -1
                End If
            End If
            Exponent >>= 1
            If CheckMul(Number, Number) Then
                Number *= Number
            Else
                Return -1
            End If
        End While
        Return Result
    End Function
    Shared Function GetBitSize(Number As BigInteger) As Integer
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
    Shared Function IntegerNthRoot(ByVal X As BigInteger, ByVal N As BigInteger) As BigInteger
        Dim UpperBound As BigInteger = BigInteger.One
        Dim pow As BigInteger = BigInteger.One
        While pow <= X 'find power of 2 upper bound
            UpperBound <<= 1
            pow = IntegerPositivePower(UpperBound, N)
            If pow = -1 Then Return -1
        End While
        Dim LowerBound As BigInteger = UpperBound / 2, Mid = BigInteger.Zero 'Binary search
        While LowerBound < UpperBound
            Mid = (LowerBound + UpperBound) / 2
            Dim MidNth As BigInteger = BigInteger.Pow(Mid, N)
            If LowerBound < Mid AndAlso MidNth < X Then
                LowerBound = Mid
            ElseIf UpperBound > Mid AndAlso MidNth > X Then
                UpperBound = Mid
            Else
                Return Mid
            End If
        End While
        Return Mid + 1
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
            Result = Factorial(Number.Solution)
            If Result <> -1 Then Solution.Add(New UnarySolution(Result, Number, UnarySolution.eUnaryOperation.eFactorial))
            'Solution.Add(New UnarySolution(Math.Exp(Number.Solution), Number, UnarySolution.eUnaryOperation.eNaturalExponent))
            'Solution.Add(New UnarySolution(Math.Log(Number.Solution), Number, UnarySolution.eUnaryOperation.eNaturalLogarithm))
            If IntegerLogarithm10(Number.Solution) <> -1 Then
                Solution.Add(New UnarySolution(IntegerLogarithm10(Number.Solution), Number, UnarySolution.eUnaryOperation.eLogarithmBase10))
            End If
            If MaxSubSolBase10 = -1 Then MaxSubSolBase10 = NumberOfDigits(MaxSubSolution)
            If Number.Solution <= MaxSubSolBase10 Then 'AndAlso IntegerPositivePower(10, Number.Solution) <> -1 Then
                Result = IntegerPositivePower(10, Number.Solution)
                If Result <> -1 Then
                    Solution.Add(New UnarySolution(Result, Number, UnarySolution.eUnaryOperation.ePowerOf10))
                End If
            End If
            Result = Factorial(Number.Solution - 1)
            If Result <> -1 Then Solution.Add(New UnarySolution(Result, Number, UnarySolution.eUnaryOperation.eGamma))
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
        If BinaryDict.ContainsKey((Number1.Solution, Number2.Solution)) Then Return BinaryDict((Number1.Solution, Number2.Solution)).Select(
            Function(sol) New BinarySolution(sol.Solution, Number1, Number2, sol.Operation)).ToArray()
        Dim Solution As New List(Of BinarySolution)
        Solution.Add(New BinarySolution(Number1.Solution + Number2.Solution, Number1, Number2, BinarySolution.eBinaryOperation.eAddition))
        Solution.Add(New BinarySolution(Number1.Solution - Number2.Solution, Number1, Number2, BinarySolution.eBinaryOperation.eSubtraction))
        If CheckMul(Number1.Solution, Number2.Solution) Then
            Solution.Add(New BinarySolution(Number1.Solution * Number2.Solution, Number1, Number2, BinarySolution.eBinaryOperation.eMultiplication))
        End If
        If Number2.Solution <> 0 AndAlso Number1.Solution Mod Number2.Solution = 0 Then 'Number2.Solution * (Number1.Solution / Number2.Solution) = Number1.Solution Then
            Solution.Add(New BinarySolution(Number1.Solution / Number2.Solution, Number1, Number2, BinarySolution.eBinaryOperation.eDivision))
        End If
        If Number2.Solution <> 0 Then
            Solution.Add(New BinarySolution(Number1.Solution Mod Number2.Solution, Number1, Number2, BinarySolution.eBinaryOperation.eModulus))
        End If
        'Prevent imaginary numbers such as negative numbers to binary reciprical powers
        If Number2.Solution >= 0 AndAlso Number2.Solution <= Int32.MaxValue Then ' AndAlso IntegerPositivePower(Number1.Solution, Number2.Solution) <> -1 Then
            Dim Result As BigInteger = IntegerPositivePower(Number1.Solution, Number2.Solution)
            If Result <> -1 Then
                Solution.Add(New BinarySolution(Result, Number1, Number2, BinarySolution.eBinaryOperation.ePower))
            End If
            'Solution.Add(New BinarySolution(Number1.Solution ^ Number2.Solution, Number1, Number2, BinarySolution.eBinaryOperation.ePower))
        End If
        If Number1.Solution = 2 Then
            Dim Result As BigInteger = IntegerSquareRoot(Number2.Solution)
            If Result * Result = Number2.Solution Then
                Solution.Add(New BinarySolution(Result, Number1, Number2, BinarySolution.eBinaryOperation.eNthRoot))
            End If
        ElseIf Number1.Solution > 2 AndAlso Number1.Solution <= Int32.MaxValue Then '1st root is an identity and multiplication would be prefered, zeroth root only has a solution for 1 but in x^(1/n) notation its impossible
            'BigInteger.Pow requires number non-negative and can fit into an Int32
            Dim Result As BigInteger = IntegerNthRoot(Number2.Solution, Number1.Solution)
            If Result <> -1 AndAlso BigInteger.Pow(Result, Number1.Solution) = Number2.Solution Then
                Solution.Add(New BinarySolution(Result, Number1, Number2, BinarySolution.eBinaryOperation.eNthRoot))
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
        BinaryDict.Add((Number1.Solution, Number2.Solution), Solution.ToArray())
        Return BinaryDict((Number1.Solution, Number2.Solution))
    End Function
    Shared Function GetCombinedSolutions(ByVal Numbers As Array()) As Solution()
        Dim Count As Integer
        Dim LeftCount As Integer
        Dim RightCount As Integer
        Dim Solutions As New List(Of Solution)
        If Numbers.Length = 1 Then Solutions.AddRange(Numbers(0))
        For Count = 1 To Numbers.Length - 1
            Dim LeftSolutions As New List(Of Solution)
            Dim RightSolutions As New List(Of Solution)
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
        Return Enumerable.GroupBy(Solutions.ToArray(), Function(KeySelector As Solution) KeySelector.Solution).Select(
            Function(Selector As IGrouping(Of BigInteger, Solution)) Selector.OrderBy(Function(KeySelector As Solution) KeySelector.CountUnaryOperations()).First()).ToArray()
    End Function
    '2 or more numbers
    Shared Function GetSolutions(ByVal Numbers As Array) As Solution()
        Dim Solutions As New List(Of Array)
        For Count = 0 To Numbers.Length - 1
            'Unary solutions
            Solutions.Add(GetUnarySolutions(New UnarySolution(Numbers(Count), Nothing, UnarySolution.eUnaryOperation.eNone)))
        Next
        'Binary combinations done both directions to simulate parenthesis
        Return GetCombinedSolutions(Solutions.ToArray())
    End Function
    Public Shared Function Solve(ByVal LeftNumbers As BigInteger(), ByVal RightNumbers As BigInteger()) As ValueTuple(Of Solution, Solution)
        'Accept Solutions from set on left and right with least unary operations
        Dim Solutions As ValueTuple(Of Solution, Solution)() = GetSolutions(LeftNumbers).GroupJoin(
                            GetSolutions(RightNumbers),
                            Function(Key As Solution) Key.Solution,
                            Function(Key As Solution) Key.Solution,
                            Function(Match As Solution, Results As IEnumerable(Of Solution)) As ValueTuple(Of Solution, Solution)
                                Return IIf(Results.Count = 0, New ValueTuple(Of Solution, Solution)(Nothing, Nothing), (Match, Results.DefaultIfEmpty(Nothing).FirstOrDefault()))
                            End Function).Where(Function(Sol As ValueTuple(Of Solution, Solution)) Not Sol.Item1 Is Nothing AndAlso Not Sol.Item2 Is Nothing).ToArray()
        If Solutions.Count = 0 Then Return (Nothing, Nothing)
        Return Solutions.OrderBy(Function(Sol As ValueTuple(Of Solution, Solution)) Sol.Item1.CountUnaryOperations() + Sol.Item2.CountUnaryOperations()).First()
    End Function
    Public Shared Function Solve(ByVal Numbers As BigInteger()) As ValueTuple(Of Solution, Solution)
        Dim Count As Integer
        Dim Solutions As ValueTuple(Of Solution, Solution) 'BigInteger(), BigInteger(), = position
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
            If Not Solutions.Item1 Is Nothing AndAlso Not Solutions.Item2 Is Nothing Then Return Solutions
        Next
        Return (Nothing, Nothing)
    End Function
End Class