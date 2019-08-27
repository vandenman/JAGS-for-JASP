//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

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
        text: "model{\n\n}"
	}

    Group
    {
        TextField { name: "nameForN";               label: qsTr("Name for sample size"); placeholderText: qsTr("n")      }
        TextField { name: "parametersMonitored";    label: qsTr("Parameters Monitored"); text: "$ALL"   }
        TextField { name: "parametersShown";        label: qsTr("Parameters Shown")    ;                }
    }

    Group
    {
        columns: 2

    
        Group
        {
            CheckBox { label: qsTr("Trace plots");              name: "plotTrace"   }
            CheckBox { label: qsTr("Density plots");            name: "plotDensity" }
            CheckBox { label: qsTr("Autocorrelation plots");    name: "plotAutoCor" }
            CheckBox { label: qsTr("Bivariate scatter plots");  name: "plotBivarHex"}
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

    Section
    {
        title: qsTr("Data")
        TextArea
        {
            title: qsTr("Enter data as an R list")
            name: "userData"
            textType: "JAGS"
        }
    }
}
