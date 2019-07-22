import QtQuick 2.8
import JASP.Controls 1.0
import JASP.Theme 1.0
import JASP.Widgets 1.0

Form
{
    columns: 1
    TextArea
	{
		title: qsTr("Enter JAGS model below")
		name: "model"
		textType: "JAGS"
	}

//    TextField { name: "nameForN"; label: qsTr("Name for sample size"); default: "n"; fieldWidth: 200 }
    
//    TextField { name: "initialValues"; label: qsTr("Name for sample size"); fieldWidth: 200 } 
    
    Group
    {
        TextField { name: "nameForN"; label: qsTr("Name for sample size")}     
        TextField { name: "parametersMonitored"; label: qsTr("Parameters Monitored") }  // defualt $ALL
        TextField { name: "parametersShown"; label: qsTr("Parameters Shown")}
    }
    
    Group
    {
        CheckBox { label: qsTr("Trace plots"); name: "plotTrace"}
        CheckBox { label: qsTr("Density plots"); name: "plotDensity"}
        CheckBox { label: qsTr("Autocorrelation plots"); name: "plotAutoCor"}
        CheckBox { label: qsTr("Bivariate scatter plots"); name: "plotBivarHex"}
    }
    
    Group
    {
        IntegerField
        {
            name: "noSamples"
            label: qsTr("No. samples")
            defaultValue: 2e3
            min: 10
            max: 1e9
        }
        IntegerField
        {
            name: "noBurnin"
            label: qsTr("No. burnin samples")
            defaultValue: 500
            min: 1
            max: 1e9
        }
        IntegerField
        {
            name: "noThinning"
            label: qsTr("thinning")
            defaultValue: 1
            min: 1
            max: 1e9
        }
        IntegerField
        {
            name: "noChains"
            label: qsTr("No. chains")
            defaultValue: 3
            min: 1
            max: 50
        }
    }
    
    Section
    {
        title: qsTr("Advanced")
        Group
        {
            CheckBox { label: qsTr("Monitor Deviance"); name: "monitorDeviance"}
            CheckBox { label: qsTr("Monitor DIC"); name: "monitorDIC"}
        }
        
//        RadioButtonGroup
//        {
//            name: "shownParameters"
//            title: qsTr("Posterior Samples")
//            RadioButton { value: "limited";  	label: qsTr("limited"); checked: true }
//            RadioButton { value: "unlimited";	label: qsTr("unlimited"); }
//        }
        
//        Group
//        {
//            IntegerField
//            {
//                name: "numShownParamsFrom"
//                label: qsTr("numShownParamsFrom")
//                defaultValue: 10
//                min: 1
//                max: 1e9
//            }
//            IntegerField
//            {
//                name: "numShownParamsTo"
//                label: qsTr("numShownParamsTo")
//                defaultValue: 100
//                min: 1
//                max: 1e9
//            }
//        }
    }
    
    Section
    {
        title: qsTr("Initial Values")
        TextArea
        {
            title: qsTr("Enter initial values as an R list")
            name: "initialValues"
            textType: "JAGS"
        }
    }
}
