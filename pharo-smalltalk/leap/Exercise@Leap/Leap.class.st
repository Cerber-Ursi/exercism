"
Reusable component to check if the given year is a leap year.

Funny enough, attempt to optimize the code by using the blocks to effectively short-circuit the calculations
don't give any boost - this variant is a bit slower instead (at least at the author's system).
Looks like the block building overhead is larger then the short-circuiting benefit.
To see it for yourself, open the Transcript window and run LeapBenchTest.
"
Class {
	#name : #Leap,
	#superclass : #Object,
	#category : #'Exercise@Leap'
}

{ #category : #check }
Leap >> leapYear: anInteger [ 
	"Tells if a given integer represents a leap year.
	It may be redirected to one of other class methods; for the reasons we chose this one, see class comments."
	
	
	^self leapYearSimple: anInteger 
]

{ #category : #check }
Leap >> leapYearShortCircuit: anInteger [ 
	"Tells if a given integer represents a leap year. This method uses a kind of short-circuiting by passing blocks to the logical operators."
	
	^(anInteger rem: 4) == 0 and: [ 
		(anInteger rem: 100) ~= 0 or: [
			(anInteger rem: 400) == 0
		]
	]
]

{ #category : #check }
Leap >> leapYearSimple: anInteger [ 
	"Tells if a given integer represents a leap year. This is an almost naive implementation, directly combining three conditions into one."

	^((anInteger rem: 4) == 0) and: (
		((anInteger rem: 100) ~= 0) or: ((anInteger rem: 400) == 0)
	)
]
