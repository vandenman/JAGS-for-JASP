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
		textType: "model"
        text: "model{\n\n}"
	}

    Group
    {

        VariablesForm
        {
            height: 200
            AvailableVariablesList  { name: "parametersList";            title: qsTr("Parameters in model");}
            AssignedVariablesList   { name: "monitoredParametersList";   title: qsTr("Monitor these parameters"); }
        }

        RadioButtonGroup
        {
            name: "showResultsFor"
            title: qsTr("Show results for")
            RadioButton { value: "monitorAllParameters";        label: qsTr("all monitored parameters"); checked: true; id:monitoredParameters  }
            RadioButton { value: "monitorSelectedParameters";   label: qsTr("selected parameters")                                              }
        }

        VariablesForm
        {
            height: 200
            visible: !monitoredParameters.checked
            AvailableVariablesList  { name: "monitoredParametersList2"; title: qsTr("Monitored parameters"); source: ["monitoredParametersList"]}
            AssignedVariablesList   { name: "parametersShown0";         title: qsTr("Show results for these parameters")}
        }

        TextField { name: "parametersMonitored";    label: qsTr("Parameters Monitored"); text: "$ALL"   }
        TextField { name: "parametersShown";        label: qsTr("Parameters Shown")    ;                }
    }

    Group
    {
        columns: 2
        Group
        {
            title: qsTr("Plots")
            CheckBox { name: "aggregateChains";        label: qsTr("Aggregate chains for densities and histograms"); checked:true   }
            CheckBox { label: qsTr("Trace");              name: "plotTrace"                             }
            CheckBox { label: qsTr("Density");            name: "plotDensity"                           }
            CheckBox { label: qsTr("Histogram");          name: "plotHistogram"                         }
            CheckBox { label: qsTr("Autocorrelation");    name: "plotAutoCor"; id: autoCorrelation
                IntegerField
                {
                    visible: autoCorrelation.checked
                    name: "noLags"
                    label: qsTr("No. lags")
                    defaultValue: 20
                    min: 1
                    max: 100
                }
                RadioButtonGroup
                {
                    visible: autoCorrelation.checked
                    name: "acfType"
                    title: qsTr("Type")
                    RadioButton { value: "acfLines";  label: qsTr("line"); checked:true }
                    RadioButton { value: "acfBars";   label: qsTr("bar")                }
                }
            }
            CheckBox { label: qsTr("Bivariate scatter");  name: "plotBivarHex"; id: bivariateScatter
                RadioButtonGroup
                {
                    visible: bivariateScatter.checked
                    name: "bivariateScatterDiagType"
                    title: qsTr("Diagonal plot type")
                    RadioButton { value: "dens";  label: qsTr("Density"); checked:true  }
                    RadioButton { value: "hist";  label: qsTr("Histogram")              }
                }
                RadioButtonGroup
                {
                    visible: bivariateScatter.checked
                    name: "bivariateScatterOffDiagType"
                    title: qsTr("Off-diagonal plot type")
                    RadioButton { value: "hex";     label: qsTr("Hexagonal histogram"); checked:true}
                    RadioButton { value: "scatter"; label: qsTr("Scatter plot")                     }
                }
            }
        }
        Group
        {
            title: qsTr("MCMC parameters")
            IntegerField
            {
                name: "noSamples"
                label: qsTr("No. samples")
                defaultValue: 2e3
                min: 10
                max: 1e9
                fieldWidth: 100
            }
            IntegerField
            {
                name: "noBurnin"
                label: qsTr("No. burnin samples")
                defaultValue: 500
                min: 1
                max: 1e9
                fieldWidth: 100
            }
            IntegerField
            {
                name: "noThinning"
                label: qsTr("Thinning")
                defaultValue: 1
                min: 1
                max: 1e9
                fieldWidth: 100
            }
            IntegerField
            {
                name: "noChains"
                label: qsTr("No. chains")
                defaultValue: 3
                min: 1
                max: 50
                fieldWidth: 100
            }
        }
    }
    
    Section
    {
        title: qsTr("Initial Values")
        TextArea
        {
            title: qsTr("Enter initial values as an R list")
            name: "initialValues"
            textType: "Rcode"
            text: qsTr("# Example of initial values for unknown mean and standard deviation:\n" +
                       "# replicate(\"No. chains\", list(list(\n" +
                       "#   mu = rnorm(1),\n" +
                       "#   sigma = abs(rnorm(1)),\n" +
                       "# )))"
                       )
        }
    }

    Section
    {
        title: qsTr("Data")
        TextArea
        {
            title: qsTr("Enter data as an R list")
            name: "userData"
            textType: "Rcode"
            text: qsTr( "# A Binomial example:\n" + 
                        "# list(\n" +
                        "#   k = 5,\n" +
                        "#   n = 10\n" +
                        "# )"
                      )
        }
    }

    Section
    {
        title: qsTr("Advanced")
        columns: 2
        TextField { name: "nameForN";               label: qsTr("Name for sample size"); placeholderText: qsTr("n")      }
        Group
        {
            CheckBox { label: qsTr("Monitor Deviance"); name: "monitorDeviance"}
            CheckBox { label: qsTr("Monitor DIC"); name: "monitorDIC"}
        }

        DropDown {
            name: "colorScheme"
            indexDefaultValue: 0
            label: qsTr("Color scheme for plots:")
            values:
            [
                { label: "Colorblind",  value: "Colorblind"},
                { label: "Viridis",     value: "Viridis"},
                { label: "Blue",        value: "blue"},
                { label: "Gray",        value: "gray"}
            ]
        }
    }
}
