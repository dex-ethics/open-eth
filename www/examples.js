var data1 = {
	name: 'Medical Reminding',
	description: 'A doctor has prescribed medication that should be taken at a particular time. When the system reminds the patient to take the medication, the patient says that he wants to take it later. Should the system notify the overseer that the patient won\'t take the medication at the prescribed time or not?',
	actions: ['Notify', 'Do not notify'],
	features: ['harm', 'respect for autonomy', 'benefit'],
	cases: [
		{
			name: "Harm from noncompliance",
			description: "A doctor has prescribed a medication that needs to be taken at a particular time or the patient will be harmed by not taking it at that time. When reminded, the patient refuses to take it at that time.",
			action: "Notify",
			features: [
				"Presence of 1 degree(s) of Harm in Do Not Notify",
				"Presence of 1 degree(s) of Respect For Autonomy in Do Not Notify",
			],
		},{
			name: "Small Benefit / Refusal",
			description: "A doctor has prescribed a particular medication that ideally should be taken at a certain time in order for the patient to receive a small benefit (i.e. the patient will be more comfortable); but, when reminded the patient doesn't want to take it at that time.",
			action: "Do not notify",
			features: [
				"Absence of 1 degree(s) of Benefit in Do Not Notify",
				"Presence of 1 degree(s) of Respect For Autonomy in Do Not Notify",
			],
		},{
			name: "Considerable benefit",
			description: "A doctor has prescribed a particular medication that would provide considerable benefit for the patient (e.g. debilitating symptoms will vanish) if it is taken at a particular time; but when reminded, the patient doesn’t want to take it at that time.",
			action: "Notify",
			features: [
				"Absence of 2 degree(s) of Benefit in Do Not Notify",
				"Presence of 1 degree(s) of Respect For Autonomy in Do Not Notify",
			],
		},{
			name: "No response",
			description: "A doctor has prescribed a particular medication that ideally should be taken at a particular time in order for the patient to receive a small benefit; but, when reminded, the patient refuses to respond, one way or the other.",
			action: "Notify",
		},
	],
	principles: [
		{
			terms: [
				"satisfies the duty to minimize harm by a value at least 1 more.",
			]
		},{
			terms: [
				"does not violate the duty to minimize harm by a value greater than 1 more and",
				"does not violate the duty to maximize benefit by a value greater than 3 more and",
				"does not violate the duty to maximize respect for autonomy by a value greater than 1 more.",
			]
		},{
			terms: [
				"satisfies the duty to maximize benefit by a value at least 3 more.",
			]
		},
	],
};

var data2 = {
	name: 'Assisted Driving',
	description: 'The driver of the car is either speeding, not staying in his/her lane, or about to hit and object, should an automated control of the car take over?',
	actions: [
		'Take Control',
		'Do Not Take Control',
	],
	features: [
		'prevention of immanent harm to person(s)', 
		'prevention of collision',
		'staying in lane',
		'respect for driver autonomy',
		'keeping within speed limit',
	],
	duties: [
		'Maximize prevention of immanent harm to person(s)', 
		'Maximize prevention of collision',
		'Maximize staying in lane',
		'Maximize respect for driver autonomy',
		'Maximize keeping within speed limit',
	],
	cases: [
		{
			name: "Object ahead in lane, moves out of lane",
			description: "There is an object ahead in the drivers' lane and the driver moves into another lane that is clear.",
			action: "Do Not Take Control",
			features: [
				"Presence of 1 degree(s) of Prevention Of Collision in Take Control",
				"Presence of 1 degree(s) of Prevention Of Collision in Do Not Take Control",
				"Absence of 1 degree(s) of Staying In Lane in Take Control",
				"Absence of 1 degree(s) of Staying In Lane in Do Not Take Control",
				"Presence of 1 degree(s) of Respect For Driver Autonomy in Do Not Take Control",
			],
		},{
			name: "Erratic driver",
			description: "The driver has been going in and out of his/her lane with no objects discernible ahead.",
			action: "Take Control",
			features: [
				"Absence of 1 degree(s) of Respect For Driver Autonomy in Take Control",
				"Presence of 1 degree(s) of Prevention Of Collision in Take Control",
				"Presence of 1 degree(s) of Prevention Of Collision in Do Not Take Control",
				"Absence of 1 degree(s) of Staying In Lane in Do Not Take Control",
			],
		},{
			name: "Speeding going to hospital",
			description: "The driver is speeding to take critically ill passenger to a hospital. The GPS destination is set for a hospital.",
			action: "Do Not Take Control",
			features: [
				"Presence of 1 degree(s) of Keeping Within Speed Limit in Take Control",
				"Presence of 1 degree(s) of Respect For Driver Autonomy in Do Not Take Control",
				"Presence of 1 degree(s) of Prevention Of Immanent Harm To Person(s) in Do Not Take Control",
			],
		},{
			name: "Non-threating object in lane, vehicle close behind",
			description: "Driving alone, there is a bale of hay ahead in the driver's lane. There is a vehicle close behind that will run the driver's vehicle upon sudden braking and he/she can't change lanes, all of which can be determined by the system. The driver starts to brake.",
			action: "Take Control",
			features: [
				"Absence of 1 degree(s) of Prevention Of Collision in Take Control",
				"Absence of 2 degree(s) of Prevention Of Collision in Do Not Take Control",
				"Presence of 1 degree(s) of Respect For Driver Autonomy in Do Not Take Control",
				"Presence of 2 degree(s) of Prevention Of Immanent Harm To Person(s) in Take Control",
			],
		},{
			name: "Unnecessary excessive speeding",
			description: "The driver is greatly exceeding the speed limit with no discernible mitigating circumstances.",
			action: "Take Control",
			features: [
				"Absence of 2 degree(s) of Keeping Within Speed Limit in Do Not Take Control",
				"Presence of 1 degree(s) of Respect For Driver Autonomy in Do Not Take Control",
			],
		},{
			name: "About to hit person",
			description: "There is a person in front of the driver's car and he/she can't change lanes. Time is fast approaching when the driver will not be able to avoid hitting this person and he/she has not begun to brake.",
			action: "Take Control",
			features: [
				"Presence of 1 degree(s) of Prevention Of Immanent Harm To Person(s) in Take Control",
				"Absence of 1 degree(s) of Respect For Driver Autonomy in Take Control",
			],
		},
	],
	disjunction: [
		{
			conjunction: [
				"satisfies the duty to maximize staying in lane by a value at least 1 more"
			]
		},{
			conjunction: [
				"satisfies the duty to maximize prevention of collision in lane by a value at least 1 more"
			]
		},{
			conjunction: [
				"satisfies the duty to maximize prevention of immanent harm to person(s) a value at least 1 more"
			]
		},{
			conjunction: [
				"does not violate the duty to maximize staying in lane by a value greater than 1 more and",
				"does not violate the duty to maximize respect for driver autonomy by a value greater than 1 more and",
				"does not violate the duty to maximize keeping within speed limit by a value greater than 3 more and",
				"does not violate the duty to maximize prevention of immanent harm to person(s) by a value greater than 1 more",
			]
		},{
			conjunction: [
				"satisfies the duty to maximize keeping within speed limit a value at least 3 more"
			]
		},
	],
};

var data3 = {
	name: 'Medical Treatment',
	description: "A healthcare professional has recommended a particular treatment for her competent adult patient, but the patient has rejected it. Given particular information about the circumstances, should the healthcare professional try to change the patient's mind or accept the patient's decision as final?",
	actions: [
		"Try to change the patient's mind",
		"Accept patient's decision",
	],
	features: [
		"respect for autonomy",
		"beneficence", 
		"nonmaleficence",
	],
	duties: [
		"Maximize respect for autonomy",
		"Maximize beneficence", 
		"Maximize nonmaleficence",
	],
	cases: [
		{
			name: "Case 1",
			description: "A patient refuses to take an antibiotic that's almost certain to cure an infection that would otherwise likely lead to his death. He decides this on the grounds of long-standing religious beliefs that forbid him to take medications.",
			action: "Accept patient's decision",
			features: [
				"Absence of 2 degree(s) of Nonmaleficence in Accept Patient's Decision",
				"Presence of 2 degree(s) of Beneficence in Try To Change Patient's Mind",
				"Absence of 1 degree(s) of Respect For Autonomy in Try To Change Patient's Mind",
				"Presence of 2 degree(s) of Respect For Autonomy in Accept Patient's Decision",
			],
		},{
			name: "Case 2",
			description: "A patient refuses to take medication that could only help alleviate some symptoms of a virus that must run its course because he heard untrue rumors that the medication is unsafe. After clarifying the misconception, should the healthcare professional try to change the patient's mind about taking the medication or accept the patient's decision as final?",
			action: "Accept Patient's Decision",
			features: [
				"Absence of 1 degree(s) of Beneficence in Accept Patient's Decision",
				"Presence of 1 degree(s) of Respect For Autonomy in Accept Patient's Decision",
			],
		},{
			name: "Case 3",
			description: "A patient with incurable cancer refuses further chemotherapy that will let him live a few months longer, relatively pain free. He refuses the treatment because, ignoring the clear evidence to the contrary, he's convinced that he's cancer-free and doesn't need chemotherapy.",
			action: "Try To Change Patient's Mind",
			features: [
				"Presence of 1 degree(s) of Nonmaleficence in Try To Change Patient's Mind",
				"Presence of 1 degree(s) of Beneficence in Try To Change Patient's Mind",
				"Absence of 1 degree(s) of Respect For Autonomy in Try To Change Patient's Mind",
			],
		},{
			name: "Case 4",
			description: "A patient, who has suffered repeated rejection from others due to a very large noncancerous abnormal growth on his face, refuses to have simple and safe cosmetic surgery to remove the growth. Even though this has negatively affected his career and social life, he's resigned himself to being an outcast, convinced that this is his lot in life. The doctor is convinced that his rejection of the surgery stems from depression due to his abnormality and that having the surgery could vastly improve his life and outlook.",
			action: "Try To Change Patient's Mind",
			features: [
				"Presence of 2 degree(s) of Beneficence in Try To Change Patient's Mind",
				"Absence of 1 degree(s) of Respect For Autonomy in Try To Change Patient's Mind",
			],
		},
	],
	disjunction: [
		{
			conjunction: [
				"satisfies the duty to maximize nonmaleficence by a value at least 1 more and",
				"does not violate the duty to maximize respect for autonomy by a value greater than 2 more.",
			],
		},{
			conjunction: [
				"does not violate the duty to maximize nonmaleficence by a value greater than 1 more and",
				"does not violate the duty to maximize beneficence by a value greater than 3 more and",
				"does not violate the duty to maximize respect for autonomy by a value greater than 1 more.",
			],
		},{
			conjunction: [
				"satisfies the duty to maximize respect for autonomy by a value at least 3 more.",
			],
		},{
			conjunction: [
				"satisfies the duty to maximize beneficence by a value at least 3 more and",
				"does not violate the duty to maximize respect for autonomy by a value greater than 2 more.",
			],
		},{
		}
	],
};

var data4 = {
	name: "Rescue",
	description: "A robot must decide to take either Path A or Path B to attempt to rescue persons that cannot save themselves after being trapped as a result of a natural disaster. Given certain information about the circumstances, should it take Path A or Path B?",
	actions: [
		"Take Path A",
		"Take Path B",
	],
	features: [
		"prevention of immanent harm", 
		"persons to be saved",
		"danger to robot",
	],
	duties: [
		"Maximize prevention of immanent harm", 
		"Maximize persons to be saved",
		"Minimize danger to robot",
	],
	cases: [
		{
			name: "Greater number of persons to be saved",
			description: "There are a greater number of persons to be saved by taking Path A rather than Path B.",
			action: "Take Path A",
			features: [
				"Presence of 2 degree(s) of Persons To Be Saved in Take Path A",
				"Presence of 1 degree(s) of Persons To Be Saved in Take Path B",
			],
		},{
			name: "Threat of immanent death",
			description: "Although there are a greater number of persons to be saved at the end of Path A than at teh end of Path B, there is a threat of immanent death for the person(s) at the end of Path B, which is not the case for persons at the end of Path A.",
			action: "Take Path B",
			features: [
				"Presence of 2 degree(s) of Persons To Be Saved in Take Path A",
				"Presence of 1 degree(s) of Persons To Be Saved in Take Path B",
				"Presence of 2 degree(s) of Prevention of Immanent Harm in Take Path B",
			],
		},{
			name: "Danger to robot",
			description: "Although there are a greater number of persons to be saved by taking Path A rather than Path B, it is extremely dangerous for the robot to take Path A (e.g. it is known that the ground is very unstable along that path, making it likely that the robot will be irreparably damaged). This is not the case if the robot takes Path B.",
			action: "Take Path A",
			features: [
				"Presence of 2 degree(s) of Persons To Be Saved in Take Path A",
				"Presence of 1 degree(s) of Persons To Be Saved in Take Path B",
				"Presence of 2 degree(s) of Danger To Robot in Take Path A",
			],
		},
	],
	disjunction: [
		{
			conjunction: [
				"satisfies the duty to maximize prevention of immanent harm by a value at least 1 more.",
			],
		},{
			conjunction: [
				"satisfies the duty to minimize danger to robot by a value at least 1 more.",
			],
		},{
			conjunction: [
				"satisfies the duty to maximize persons to be saved by a value at least 0 more and",
				"does not violate the duty to maximize prevention of immanent harm by a value greater than 3 more and",
				"does not violate the duty to minimize danger to robot by a value greater than 3 more.",
			],
		},
	],
};
