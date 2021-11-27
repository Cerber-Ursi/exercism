"
One for X, one for me.

Here X can be passed into ""who:"" message or used implicitly in ""who"" message; in the last case it equals 'you'.
"
Class {
	#name : #TwoFer,
	#superclass : #Object,
	#category : #'Exercise@TwoFer'
}

{ #category : #getter }
TwoFer >> who [
	^self who: 'you'
]

{ #category : #setter }
TwoFer >> who: aString [ 
	^'One for ', aString, ', one for me.'
]
