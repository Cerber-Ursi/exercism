"
Benchmark for Leap class.
"
Class {
	#name : #LeapBenchTest,
	#superclass : #TestCase,
	#instVars : [
		'calculator',
		'years'
	],
	#category : #'Exercise@Leap'
}

{ #category : #benchmarking }
LeapBenchTest >> bench: aBlockClosure [ 
	| total |

	Smalltalk garbageCollect.
	total := 0.0.
	1 to: 10000 do: [ :iteration |
		total := total + (Time microsecondsToRun: [ years do: aBlockClosure ])
	].
	Transcript show: 'Average: '.
	Transcript show: total / 10000.0.
	Transcript show: ' microseconds per iteration'.
	Transcript cr.
]

{ #category : #running }
LeapBenchTest >> setUp [
	super setUp. 
	calculator := Leap new.
	years := Interval from: 1 to: 2000
]

{ #category : #tests }
LeapBenchTest >> test_runShortCircuit [
	Transcript show: 'Benchmarking short-circuit code...'.
	Transcript cr.
	self bench: [ :val | calculator leapYearShortCircuit: val ].
	^self assert: true
]

{ #category : #tests }
LeapBenchTest >> test_runSimple [
	Transcript show: 'Benchmarking straightforward code...'.
	Transcript cr.
	self bench: [ :val | calculator leapYearSimple: val ].
	^self assert: true
]
