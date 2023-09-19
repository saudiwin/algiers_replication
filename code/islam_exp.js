var myLanguage = "${e://Field/Q_Language}";
var myCountry = "${e://Field/country}";

if (Math.random() >= 0.5) {
	// need list of districts by country
	var district = "${q://QID88/ChoiceGroup/AllChoices?displayLogic=0}";
	district = district.split(","); // convert to array
	district = district[Math.floor(Math.random() * district.length)];
} else {
	var district = "${q://QID88/ChoiceGroup/SelectedChoices}";
}

if (myLanguage == "EN") {
	// Create Variables for Traits associated with each dimension.
	var name = ["Tarek", "Mohammed", "Abdelaziz", "Amira", "Khadija"];
	var age = Math.floor(Math.random() * 65 + 20);

	if (Math.random() >= 0.1) {
		// need list of districts by country
		if (Math.random() >= 0.5) {
			var religion = "Muslim";
		} else {
			var religion = "Devout Muslim";
		}


	} else {
		var religion = "Christian";
	}
	if (Math.random() >= 0.3) {
		// need list of districts by country
		var language = "Arabic";
	} else {
		var language = "Amazigh"; // if algerian
	}
	var job = ["Unemployed", "Doctor", "Lawyer", "Engineer", "Businessman",
		"Military officer", "Farmer"
	];
	var civil = ["Trade unionist", "Imam",
		"Human rights activist",
		"Member of an Islamic charitable organization",
		"Member of a charitable organization"
	];
	var ideology = ["Islamist",
		"Salafist",
		"Liberal",
		"Leftist",
		"Independent"
	];

	var background1 = ["Member of the National Liberation Front",
		"Member of an opposition party that ran in past elections",
		"Member of an opposition party that boycotted past elections",
		"Independent opposition activist that boycotted past elections",
		"Not involved in politics"
	];

	var background2 = ["In prison 1 year for protesting",
		"In prison 10 years for protesting",
		"Lived in exile in Saudi Arabia",
		"Lived in exile in the U.S.",
		"Lived in Algeria"
	];

	var social = ["Ensuring laws conform to the Sharia",
		"Establishing a civil state",
		"Expanding rights for women",
		"Expanding rights for minorities",
		"Enacting Arabization policies"
	];

	var econ = ["Redistributing wealth to poorer citizens",
		"Opening the economy to the global market",
		"Fighting corruption",
		"Providing government jobs and services to their home district"
	];

	var revolution = ["Prosecuting former regime officials for their crimes",
		"Advancing the transition to democracy",
		"Restoring security and stability",
		"Forgiving former regime officials to allow the country to heal"
	];

	var motivation = ["To help protect the gains of the revolution",
		"To help Algeria develop as a modern nation",
		"To help citizens become closer to Allah",
		"To improve the lives of Algerian citizens",
		"To improve the lives of members of their district"
	];


} else if (myLanguage == "AR") {

	var name = ["طارق", "محمد", "عبد العزيز", "أميرة", "خديجة"];
	var age = Math.floor(Math.random() * 65 + 20);

	if (Math.random() >= 0.1) {
		// need list of districts by country
		if (Math.random() >= 0.5) {
			var religion = "مسلم";
		} else {
			var religion = "مسلم ملتزم";
		}

	} else {
		var religion = "مسيحي";
	}

	if (Math.random() >= 0.3) {
		// need list of districts by country
		var language = "عربية";
	} else {
		var language = "أمازيغية"; // if algerian
	}
	var job = ["بطال", "طبيب", "محامس", "مهندس", "رجل أعمال", "مسؤول عسكري",
		"مزارع"
	];
	var civil = ["عضو في اتحاد تجاري", "إمام",
		"ناشط في حقوق الإنسان",
		"عضو في جمعية خيرية إسلامية",
		"عضو في جمعية خيرية"
	];
	var ideology = ["إسلامي",
		"سلفي",
		"ليبرالي",
		"يساري",
		"مستقل"
	];

	var background1 = ["عضو في جبهة الاستقلال الوطني",
		"عضو في حزب معارض ترشح في انتخابات سابقة",
		"عضو في حزب معارض قاطع انتخابات سابقة",
		"ناشط معارض مستقل قاطع انتخابات سابقة",
		"غير منخرط في السياسة"
	];

	var background2 = ["مسجون لمدة 1 سنة للتظاهر",
		"مسجون لمدة 10 سنوات للتظاهر",
		"عاش في المنفى في السعودية",
		"عاش في المنفى في الولايات المتحدة الأمريكية",
		"عاش في الجزائر"
	];

	var social = ["التأكد من انطباق القوانين مع الشريعة",
		"إقامة دولة مدنية",
		"توسيع الحقوق للنساء",
		"توسيع الحقوق للأقليات",
		"سن سياسات التعريب"
	];

	var econ = ["إعادة توزيع الثروة على المواطنين الأكثر فقراً",
		"فتح الاقتصاد على السوق العالمية",
		"محاربة الفساد",
		"توفير خدمات ووظائف حكومية لمقاطعتهم الأم"
	];

	var revolution = ["مقاضاة مسؤولي النظام السابقين على جرائمهم",
		"تقديم المشورة للانتقال إلى الديمقراطية",
		"استعادة الأمن والاستقرار",
		"مسامحة مسؤولي النظام القديم للسماح للبلاد بالشفاء"
	];

	var motivation = ["للمساعدة في حماية مكاسب الثورة",
		"لمساعدة الجزائر في تطوير دولة حديثة",
		"لمساعدة المواطنين في التقرب من الله",
		"لتطوير حياة المواطنين الجزائريين",
		"لتطوير حياة أعضاء مقاطعتهم"
	];

	if (Math.random() >= 0.1) {
		// need list of districts by country
		var religion = "مسلم";
	} else {
		var religion = "مسيحي";
	}
	if (Math.random() >= 0.3) {
		// need list of districts by country
		var language = "عربية";
	} else {
		var language = "أمازيغية"; // if algerian
	}

} else if (myLanguage == "FR") {

	var name = ["Tarek", "Mouhammed", "Abdelaziz", "Amira", "Khadidja"];
	var age = Math.floor(Math.random() * 65 + 20);

	if (Math.random() >= 0.1) {
		// need list of districts by country
		if (Math.random() >= 0.5) {
			var religion = "Musulman";
		} else {
			var religion = "Musulman dévot";
		}

	} else {
		var religion = "Chrétien";
	}

	if (Math.random() >= 0.3) {
		// need list of districts by country
		var language = "Arabe";
	} else {
		var language = "Amazigh"; // if algerian
	}
	var job = ["Sans empl", "Médecin", "Avocat", "Ingénieur", "Homme d'affaires",
		"Officier militaire", "Fermier"
	];
	var civil = ["Syndicaliste", "Imam",
		"Militant des droits de l'homme",
		"Membre d'une organisation caritative islamique",
		"Membre d'un organisme de bienfaisance"
	];
	var ideology = ["Islamiste",
		"Salafiste",
		"Libéral",
		"Gauchiste",
		"Indépendant"
	];

	var background1 = ["Membre du Front de libération nationale",
		"Membre d'un parti d'opposition ayant participé aux dernières élections",
		"Membre d'un parti de l'opposition qui a boycotté les élections précédentes",
		"Un militant indépendant qui a boycotté les dernières élections",
		"Pas impliqué dans la politique"
	];

	var background2 = ["En prison 1 an pour avoir protesté",
		"En prison 10 ans pour avoir protesté",
		"Vécu en exil en Arabie Saoudite",
		"Vécu en exil aux Etats-Unis",
		"Vécu en Algérie"
	];

	var social = ["S'assurer que les lois sont conformes à la charia",
		"Etablir un état civil",
		"Développer les droits des femmes",
		"Élargir les droits des minorités",
		"Promulguer des politiques d'arabisation"
	];

	var econ = ["Redistribuer la richesse aux citoyens les plus pauvres",
		"Ouvrir l'économie au marché mondial",
		"Lutter contre la corruption",
		"Fournir des emplois et des services gouvernementaux à leur district d'origine"
	];

	var revolution = [
		"Poursuivre les anciens responsables du régime pour leurs crimes",
		"Faire progresser la transition vers la démocratie",
		"Restaurer la sécurité et la stabilité",
		"Pardon aux responsables de l'ancien régime pour permettre au pays de guérir"
	];

	var motivation = ["Pour aider à protéger les acquis de la révolution",
		"Pour aider l'Algérie à se développer en tant que nation moderne",
		"Pour aider les citoyens à se rapprocher d'Allah",
		"Pour améliorer la vie des citoyens algériens",
		"Pour améliorer la vie des membres de leur district"
	];

	if (Math.random() >= 0.1) {
		// need list of districts by country
		var religion = "Musulman";
	} else {
		var religion = "Chrétien";
	}
	if (Math.random() >= 0.3) {
		// need list of districts by country
		var language = "Arabe";
	} else {
		var language = "Amazigh"; // if algerian
	}


}



// Use math.random to randomly select traits for each dimension for candidate A
traits_a = [name[Math.floor(Math.random() * name.length)],
	age,
	district,
	religion,
	language,
	job[Math.floor(Math.random() * job.length)],
	civil[Math.floor(Math.random() * civil.length)],
	ideology[Math.floor(Math.random() * ideology.length)],
	background1[Math.floor(Math.random() * background1.length)],
	background2[Math.floor(Math.random() * background2.length)],
	social[Math.floor(Math.random() * social.length)],
	econ[Math.floor(Math.random() * econ.length)],
	revolution[Math.floor(Math.random() * revolution.length)],
	motivation[Math.floor(Math.random() * motivation.length)]
];

// reset religion, language and district
age = Math.floor(Math.random() * 65 + 20);
if (Math.random() >= 0.5) {
	// need list of districts by country
	district = "${q://QID88/ChoiceGroup/AllChoices?displayLogic=0}";
	district = district.split(","); // convert to array
	district = district[Math.floor(Math.random() * district.length)];
} else {
	district = "${q://QID88/ChoiceGroup/SelectedChoices}";
}

if (myLanguage == "EN") {
	if (Math.random() >= 0.1) {
		// need list of districts by country
		religion = "Muslim";
	} else {
		religion = "Christian";
	}
	if (Math.random() >= 0.3) {
		// need list of districts by country
		language = "Arabic";
	} else {
		language = "Amazigh"; // if algerian
	}
} else {
	if (Math.random() >= 0.1) {
		// need list of districts by country
		religion = "مسلم";
	} else {
		religion = "مسيحي";
	}
	if (Math.random() >= 0.3) {
		// need list of districts by country
		language = "عربية";
	} else {
		language = "أمازيغية"; // if algerian
	}
}


// Use math.random to randomly select traits for each dimension for candidate B
traits_b = [name[Math.floor(Math.random() * name.length)],
	age,
	district,
	religion,
	language,
	job[Math.floor(Math.random() * job.length)],
	civil[Math.floor(Math.random() * civil.length)],
	ideology[Math.floor(Math.random() * ideology.length)],
	background1[Math.floor(Math.random() * background1.length)],
	background2[Math.floor(Math.random() * background2.length)],
	social[Math.floor(Math.random() * social.length)],
	econ[Math.floor(Math.random() * econ.length)],
	revolution[Math.floor(Math.random() * revolution.length)],
	motivation[Math.floor(Math.random() * motivation.length)]
];

// Create list of variables to use when setting attributes
a_list = ["a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11",
	"a12", "a13", "a14"
];
b_list = ["b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11",
	"b12", "b13", "b14"
];

// set html values in conjoint table
for (i = 0; i < 14; i++) {
	document.getElementById(a_list[i]).innerHTML = traits_a[i];
	document.getElementById(b_list[i]).innerHTML = traits_b[i];
}

// store values as embedded data fields
Qualtrics.SurveyEngine.setEmbeddedData('traits1a', traits_a.join("|"));
Qualtrics.SurveyEngine.setEmbeddedData('traits1b', traits_b.join("|"));
