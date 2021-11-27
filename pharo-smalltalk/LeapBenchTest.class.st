{ #category : #tests }
LeapBenchTest >> test_runShortCircuit [
	"Transcript show: 'Benchmarking short-circuit code...'."
	"Transcript cr."
	"self bench: [ :val | calculator leapYearShortCircuit: val ]."
	"^self assert: true"
]
