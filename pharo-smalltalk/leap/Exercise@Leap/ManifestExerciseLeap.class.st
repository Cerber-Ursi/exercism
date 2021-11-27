"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestExerciseLeap,
	#superclass : #PackageManifest,
	#category : #'Exercise@Leap-Manifest'
}

{ #category : #'code-critics' }
ManifestExerciseLeap class >> ruleRBCodeCruftLeftInMethodsRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#LeapBenchTest)) #'2019-06-12T19:08:31.312787+07:00') )
]
