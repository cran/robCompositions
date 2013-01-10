#library(robCompositions)



robGUI = function() {
	gtkHBoxNew <- gtkAlignmentNew <- gtkWindowNew <- gtkFrameNew <- 
	gtkComboBoxNewText <- gtkLabelNew <- gtkEntryNew <- 
	gtkRadioButtonNewWithLabel <- gtkButtonNewWithLabel <- 
	gtkComboBoxEntryNewText <- gtkTextViewNew <- gtkScrolledWindowNew <- 
	gtkMessageDialog <- gtkFileChooserDialogNew <- GtkResponseType <- 
    gtkFileChooserDialogNew <- GtkResponseType <- gtkFileChooserDialogNew <- 
	GtkResponseType <- gtkMenuBarNew <- gtkMenuItemNewWithLabel <- 
	gtkMenuNew <- gtkFileChooserDialogNew <- GtkResponseType <- 
	gtkFileChooserDialogNew <-  gtkVBoxNew <-  gSignalConnect <- 
	GtkResponseType <- NULL
	
# access robGUIenv
robGUIenv <- function() {
	get("robGUIenvir", envir=as.environment("package:VIM"))
}	

	# check if RGtk2 is installed, it's not in dependencies, it's big and not necessary for statistics itself
	
	is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
	
	if(!is.installed('RGtk2')) warning('RGtk2 has to be installed first.')
	
	else {
		
		cat('\n ... load the package RGtk2 first by library(RGtk2).\n')
		
#		library(RGtk2)
		
	}
	
	
	
	# function to initialize combo boxes, given vector of strings
	
	appendTexts = function(cont, texts) {
		
		for(text in texts) cont$appendText(text)
		
	}
	
	
	
	# function to layout widgets to vertical or horizontal box
	
	boxObject = function(type, homogenous, spacing, children, expand, fill, padding) {
		
		if(type == "h") box = gtkHBoxNew(homogenous, spacing)
		
		else box = gtkVBoxNew(homogenous,spacing)
		
		i = 1
		
		for(child in children) {
			
			box$packStart(child, expand[i], fill[i], padding[i])
			
			i = i+1
			
		}
		
		box
		
	}
	
	
	
	# aligns widget and returns gtkAlignment widget
	
	align = function(object, xalign, yalign, xscale, yscale) {
		
		alignment = gtkAlignmentNew(xalign, yalign, xscale, yscale)
		
		alignment$add(object)
		
		alignment
		
	}
	
	
	
	# using gtkAlignment to specify padding of the widget, returns gtkAlignment
	
	padding = function(object, top, bottom, left, right) {
		
		alignment = gtkAlignmentNew()
		
		alignment$add(object)
		
		alignment$setPadding(top, bottom, left, right)
		
		alignment
		
	}
	
	
	
	# eliminates null value by length check
	
	nonnull = function(value) { if(length(value)<1) return("") else return(value) }
	
	
	
	# selects particular string in the combobox
	
	# TODO make this function more simple, it's too complicated for such a simple purpose
	
	setActiveText = function(combobox, text) {
		
		iter = combobox$getModel()$iterChildren()
		
		status = iter$retval
		
		iter = iter$iter
		
		
		
		while(status == TRUE) {
			
			value = combobox$getModel()$getValue(iter, 0)$value
			
			if(value == text) {
				
				combobox$setActiveIter(iter)
				
				return(TRUE)
				
			}
			
			status = combobox$getModel()$iterNext(iter)
			
		}
		
		combobox$setActive(-1)
		
		return(FALSE)
		
	}
	
	
	
	# state based port for show() and hide()
	
	setShown = function(object, show) {
		
		if(show) object$show()
		
		else object$hide()
		
	}
	
	
	
	# help strings, it would be better to get them directly from help files, but it's much more simplier and portable to specify them in code
	
	helps = c("Choose composition",
			
			"Closes compositions to sum up to a given constant (default 1), by dividing each part of a composition by its row sum.",
			
			"The transformation moves D-part compositional data from the simplex into a (D-1)-dimensional real space.",
			
			"Inverse log-ratio transformation, often called logistic transformation.",
			
			"Computes the Aitchison distance between two observations or between two data sets.",
			
			"Outlier detection for compositional data using standard and robust statistical methods.",
			
			"The compositional data set is transformed using the ilr tranformation.  Afterwards, robust principal component analysis is performed.  Resulting loadings and scores are back-transformed to the clr space where  the compositional biplot can be shown.",
			
			"First, the data is transformed using the ilr-transformation. After applying this transformation\n- all (D-1)-dimensional marginal, univariate distributions are tested using the univariate Anderson-Darling test for normality.\n- all 0.5 (D-1)(D-2)-dimensional bivariate angle distributions are tested using the Anderson-Darling angle test for normality.\n- the (D-1)-dimensional radius distribution is tested using the Anderson-Darling radius test for normality."
	
	)
	
	
	
	# base window construction
	
	window = gtkWindowNew(NULL, FALSE)
	
	window$setTitle("robCompositions GUI")
	
	
	
	# composition frame
	
	compositionFrame = gtkFrameNew("Composition")
	
	compositionFrame$setBorderWidth(5)
	
	
	
	composition = gtkComboBoxNewText()
	
	composition$show()
	
	appendTexts(composition, c("Closure operation", "Logratio transformation", "Inverse logration transformation", "Aitchison distance", "Outlier detection", "Robust principal component analysis", "Anderson-Darling tests"))
	
	
	
	compositionVariant = gtkComboBoxNewText()
	
	compositionVariant$show()
	
	appendTexts(compositionVariant, c("additive", "centered", "isometric"))
	
	compositionVariantBox = boxObject("h", F, 0, c(padding(gtkLabelNew("variant:"),0,0,10,5), compositionVariant), c(T,T), c(T,T), c(0,0))
	
	
	
	compositionHelp = gtkLabelNew("napoveda")
	
	compositionHelp$setLineWrap(T)
	
	compositionSelect = boxObject("h", F, 0, c(align(composition,0,0,1,0), align(compositionVariantBox,1,0,0,0)), c(T,F), c(T,F), c(0,0))
	
	
	
	constSum = gtkEntryNew()
	
	compositionConstSum = boxObject("h", F, 0, c(padding(gtkLabelNew("closure constant:"),0,0,0,5), constSum), c(F,F), c(F,T), c(0,0))
	
	
	
	ivar = gtkEntryNew()
	
	compositionIvar = boxObject("h", F, 0, c(padding(gtkLabelNew("index of the rationing part:"),0,0,0,5), ivar), c(F,T), c(F,T), c(0,0))
	
	
	
	quantile = gtkEntryNew()
	
	compositionQuantile = boxObject("h", F, 0, c(padding(gtkLabelNew("quantile:"),0,0,0,5), quantile), c(F,F), c(F,T), c(0,0))
	
	
	
	significanceLevel = gtkEntryNew()
	
	significanceLevelBox = boxObject("h", F, 0, c(padding(gtkLabelNew("significance level:"),0,0,0,5), significanceLevel), c(F,F), c(F,T), c(0,0))
	
	
	
	numberSimulations = gtkEntryNew()
	
	numberSimulationsBox = boxObject("h", F, 0, c(padding(gtkLabelNew("number of simulations:"),0,0,0,5), numberSimulations), c(F,F), c(F,T), c(0,0))
	
	
	
	methodRobust = gtkRadioButtonNewWithLabel(NULL, "robust")
	
	methodStandard = gtkRadioButtonNewWithLabel(methodRobust$getGroup(), "standard")
	
	methodBox = boxObject("h", F, 0, c(gtkLabelNew("method:"), methodRobust, methodStandard), c(F,T,T), c(F,F,F), c(0,0,0))
	
	
	
	compositionBox = boxObject("v", F, 0, c(compositionSelect, padding(compositionConstSum,10,0,0,0), padding(compositionIvar,10,0,0,0), padding(compositionQuantile,10,0,0,0), padding(significanceLevelBox,10,0,0,0), padding(numberSimulationsBox,0,0,0,0), methodBox, align(padding(compositionHelp,10,0,0,0),0,0,0,0)), c(F,T,T,T,T,T,T,F), c(F,T,T,T,T,T,T,F), c(0,0,0,0,0,0,0,0))
	
	compositionBox$setBorderWidth(5)
	
	compositionFrame$add(compositionBox)
	
	
	
	# input
	
	inputFrame = gtkFrameNew("Input")
	
	inputFrame$setBorderWidth(5)
	
	
	
	inputFileRadio <- gtkRadioButtonNewWithLabel(NULL, "File:")
	
	inputBrowse = gtkButtonNewWithLabel("Browse")
	
	inputVariableRadio <- gtkRadioButtonNewWithLabel(inputFileRadio$getGroup(), "Data:")
	
	
	
	inputFile = gtkEntryNew()
	
	inputVariable = gtkComboBoxEntryNewText()
	
	inputVariable$show()
	
	
	
	inputFileBox = boxObject("h", F, 0, c(inputFileRadio, inputFile, inputBrowse), c(F,T,F), c(F,T,F), c(0,0,0))
	
	inputVariableBox = boxObject("h", F, 0, c(inputVariableRadio, inputVariable), c(F,T), c(F,T), c(0,0))
	
	
	
	inputBox = boxObject("v", F, 0, c(inputFileBox,inputVariableBox), c(T,T), c(T,T), c(0,0))
	
	inputBox$setBorderWidth(5)
	
	inputFrame$add(inputBox)
	
	
	
	# second input for aDist
	
	input2Frame = gtkFrameNew("Input, second dataset")
	
	input2Frame$setBorderWidth(5)
	
	
	
	input2FileRadio <- gtkRadioButtonNewWithLabel(NULL, "File:")
	
	input2Browse = gtkButtonNewWithLabel("Browse")
	
	input2VariableRadio <- gtkRadioButtonNewWithLabel(input2FileRadio$getGroup(), "Data:")
	
	
	
	input2File = gtkEntryNew()
	
	input2Variable = gtkComboBoxEntryNewText()
	
	input2Variable$show()
	
	
	
	input2FileBox = boxObject("h", F, 0, c(input2FileRadio, input2File, input2Browse), c(F,T,F), c(F,T,F), c(0,0,0))
	
	input2VariableBox = boxObject("h", F, 0, c(input2VariableRadio, input2Variable), c(F,T), c(F,T), c(0,0))
	
	
	
	input2Box = boxObject("v", F, 0, c(input2FileBox,input2VariableBox), c(T,T), c(T,T), c(0,0))
	
	input2Box$setBorderWidth(5)
	
	input2Frame$add(input2Box)
	
	
	
	# output
	
	outputFrame = gtkFrameNew("Output")
	
	outputFrame$setBorderWidth(5)
	
	
	
	outputFileRadio <- gtkRadioButtonNewWithLabel(NULL, "File:")
	
	outputBrowse = gtkButtonNewWithLabel("Browse")
	
	outputVariableRadio <- gtkRadioButtonNewWithLabel(outputFileRadio$getGroup(), "Variable:")
	
	outputConsoleRadio = gtkRadioButtonNewWithLabel(outputFileRadio$getGroup(), "Console")
	
	
	
	outputFile = gtkEntryNew()
	
	outputVariable = gtkEntryNew()
	
	
	
	outputFileBox = boxObject("h", F, 0, c(outputFileRadio, outputFile, outputBrowse), c(F,T,F), c(F,T,F), c(0,0,0))
	
	outputVariableBox = boxObject("h", F, 0, c(outputVariableRadio, outputVariable), c(F,T), c(F,T), c(0,0))
	
	
	
	outputTypeCombo = gtkComboBoxNewText()
	
	outputTypeCombo$show()
	
	outputType = boxObject("h", F, 0, c(outputTypeCombo), c(T), c(T), c(0,0))
	
	
	
	outputBox = boxObject("v", F, 0, c(padding(outputType,0,10,0,0),outputFileBox,outputVariableBox,outputConsoleRadio), c(T,T,F), c(T,T,F), c(0,0,0))
	
	outputBox$setBorderWidth(5)
	
	outputFrame$add(outputBox)
	
	
	
	#output code
	
	outputCode = gtkFrameNew("Output code")
	
	outputCode$setBorderWidth(5)
	
	
	
	outputCodeView = gtkTextViewNew()
	
	outputCodeScroll = gtkScrolledWindowNew()
	
	outputCodeScroll$add(outputCodeView)
	
	outputCodeButton = gtkButtonNewWithLabel("Run code")
	
	
	
	outputCodeBox = boxObject("v", F, 0, c(outputCodeScroll, align(boxObject("h", F, 0, c(outputCodeButton), c(F), c(F), c(0)),1,0,0,0)), c(T,F), c(T,F), c(0,0))
	
	outputCodeBox$setBorderWidth(5)
	
	outputCode$add(outputCodeBox)
	
	
	
	#auto switch radio
	
	gSignalConnect(inputFile, "changed", function(d) {inputFileRadio$setActive(TRUE)})
	
	gSignalConnect(inputVariable, "changed", function(d) {inputVariableRadio$setActive(TRUE)})
	
	gSignalConnect(input2File, "changed", function(d) {input2FileRadio$setActive(TRUE)})
	
	gSignalConnect(input2Variable, "changed", function(d) {input2VariableRadio$setActive(TRUE)})
	
	gSignalConnect(outputFile, "changed", function(d) {outputFileRadio$setActive(TRUE)})
	
	gSignalConnect(outputVariable, "changed", function(d) {outputVariableRadio$setActive(TRUE)})
	
	
	
	# run button
	
	runButton = gtkButtonNewWithLabel("Run")
	
	buttonAlign = align(runButton,0,1,1,0)
	
	
	
	# universal error messagebox
	
	error <- function(msg) {
		
		dialog <- gtkMessageDialog(window, "destroy-with-parent", "error", "close", msg)
		
		dialog$run()
		
		dialog$destroy()
		
	}
	
	
	
	# browse button logic, file choosers
	
	browseInputSignal = function(d) {
		
		dialog <- gtkFileChooserDialogNew("Select input data file", NULL, "open",
				
				"gtk-cancel", GtkResponseType["cancel"],
				
				"gtk-open", GtkResponseType["accept"])
		
		if (dialog$run() == GtkResponseType["accept"]) {
			
			inputFile$setText(dialog$getFilename())
			
		}
		
		dialog$destroy()
		
	}
	
	gSignalConnect(inputBrowse, "clicked", browseInputSignal)
	
	
	
	browseInput2Signal = function(d) {
		
		dialog <- gtkFileChooserDialogNew("Select second input data file", NULL, "open",
				
				"gtk-cancel", GtkResponseType["cancel"],
				
				"gtk-open", GtkResponseType["accept"])
		
		if (dialog$run() == GtkResponseType["accept"]) {
			
			input2File$setText(dialog$getFilename())
			
		}
		
		dialog$destroy()
		
	}
	
	gSignalConnect(input2Browse, "clicked", browseInput2Signal)
	
	
	
	browseOutputSignal <- function(d) {
		
		dialog <- gtkFileChooserDialogNew("Select output data file", NULL, "save",
				
				"gtk-cancel", GtkResponseType["cancel"],
				
				"gtk-open", GtkResponseType["accept"])
		
		if (dialog$run() == GtkResponseType["accept"]) {
			
			outputFile$setText(dialog$getFilename())
			
		}
		
		dialog$destroy()
		
	}
	
	gSignalConnect(outputBrowse, "clicked", browseOutputSignal)
	
	
	
	# code run button, runs the code in text area and check for errors
	
	runCodeSignal <- function(d) {
		
		buff = outputCodeView$getBuffer()
		
		text = buff$getText(buff$getStartIter()$iter, buff$getEndIter()$iter)
		
		
		
		ret = try(eval(parse(text=text)))
		
		if(class(ret) == "try-error") return(error(ret))
		
	}
	
	gSignalConnect(outputCodeButton, "clicked", runCodeSignal)
	
	
	
	# hiding composition logic
	
	compositionChanged = function(d) {
		
		index = composition$getActive()
		
		variantIndex = compositionVariant$getActive()
		
		
		
		setShown(compositionVariantBox, index == 1 || index == 2)
		
		setShown(compositionConstSum, index == 0)
		
		setShown(compositionIvar, (index == 1 || index == 2) && variantIndex == 0)
		
		setShown(compositionQuantile, index == 4)
		
		setShown(methodBox, index == 4 || index == 5 || index == 6)
		
		setShown(input2Frame, index == 3)
		
		setShown(significanceLevelBox, index == 6)
		
		setShown(numberSimulationsBox, index == 6)
		
		
		
		if(index >= 0) compositionHelp$setText(helps[index+2])
		
		else compositionHelp$setText(helps[1])
		
		
		
		while(outputTypeCombo$getModel()$iterNChildren() != 0) outputTypeCombo$removeText(0)
		
		
		
		append = c()
		
		if(index == 0) append = c("The data for which the row sums are equal to 'const'.")
		
		else if(index == 1) {
			
			if(variantIndex == 0) append = c("the transformed data", "the rationing variable")
			
			else if(variantIndex == 1) append = c("clr transformed data", "the geometric means of the original composition")
			
			else if(variantIndex == 2) append = c("the ilr transformed data")
			
		} else if(index == 2) {
			
			if(variantIndex == 0) append = c("the transformed data matrix")
			
			else if(variantIndex == 1) append = c("the transformed data set")
			
			else if(variantIndex == 2) append = c("the transformed data")
			
		} else if(index == 3) append = c("the Aitchison distance.")
		
		else if(index == 4) append = c("resulting Mahalanobis distance", "logical vector indicating outliers and non-outliers", "graph")
		
		else if(index == 5) append = c("scores in clr space", "loadings in clr space", "eigenvalues of the clr covariance matrix", "graph")
		
		else if(index == 6) append = c("summary") 
		
		
		
		appendTexts(outputTypeCombo, append)
		
		if(length(append) > 0) outputTypeCombo$setActive(0)
		
	}
	
	gSignalConnect(composition, "changed", compositionChanged);
	
	gSignalConnect(compositionVariant, "changed", compositionChanged);
	
	
	
	# evaluates the state of changeable components and updates the code text area, it's separated from the code for 'Run' button, because additional error checks are performed there.
	
	updateCode <- function(d) {
		
		str = c("", "", "", "", "")
		
		index = composition$getActive()
		
		variant = compositionVariant$getActive()
		
		type = outputTypeCombo$getActive()
		
		
		
		if(inputFileRadio$getActive() == TRUE) str[1] = paste("x = read.table(\"", inputFile$getText(), "\")", collapse = "", sep="")
		
		else if(inputVariableRadio$getActive() == TRUE) str[1] = paste("if(!exists(\"",inputVariable$getActiveText(),"\")) data(list=\"", inputVariable$getActiveText(),"\")\nx = get(\"", inputVariable$getActiveText(), "\")", collapse="", sep="")
		
		
		
		if(input2Frame$getVisible() == TRUE) {
			
			if(input2FileRadio$getActive() == TRUE) str[2] = paste("y = read.table(\"", input2File$getText(), "\")", collapse = "", sep="")
			
			else if(input2VariableRadio$getActive() == TRUE) str[2] = paste("if(!exists(\"",input2Variable$getActiveText(),"\")) data(list=\"", input2Variable$getActiveText(),"\")\ny = get(\"", input2Variable$getActiveText(), "\")", collapse="", sep="")
			
		}
		
		
		
		if(index == 0) {
			
			constant = constSum$getText()
			
			if(nchar(constant)<1) str[3] = "result = constSum(x)"
			
			else str[3] = paste("result = constSum(x,",constant,")", collapse = "", sep = "")
			
		}
		
		else if(index == 1) {
			
			if(variant == 0) {
				
				indexVar = ivar$getText()
				
				if(nchar(indexVar)<1) str[3] = "result = addLR(x)"
				
				else str[3] = paste("result = addLR(x, ",indexVar,")", collapse="", sep="")
				
			}
			
			else if(variant == 1) str[3] = "result = cenLR(x)"
			
			else if(variant == 2) str[3] = "result = isomLR(x)"
			
		} else if(index == 2) {
			
			if(variant == 0) {
				
				indexVar = ivar$getText()
				
				if(nchar(indexVar)<1) str[3] = "result = addLRinv(x))"
				
				else str[3] = paste("result = addLR(x, ",indexVar,")", collapse = "", sep = "")
				
			}
			
			else if(variant == 1) str[3] = "result = cenLRinv(x)"
			
			else if(variant == 2) str[3] = "result = isomLRinv(x)"
			
		} else if(index == 3) {
			
			str[3] = "result = aDist(x, y)"
			
		} else if(index == 4) {
			
			quant = quantile$getText()
			
			if(methodRobust$getActive()) meth = "robust"
			
			else meth = "standard"
			
			if(nchar(quant)>0) str[3] = paste("result = outCoDa(x, method=\"",meth,"\", quantile=",quant,")", collapse = "", sep = "")
			
			else str[3] = paste("result = outCoDa(x, method=\"",meth,"\")", collapse = "", sep = "")
			
		} else if(index == 5) {
			
			if(methodRobust$getActive()) meth = "robust"
			
			else meth = "standard"
			
			str[3] = paste("result = pcaCoDa(x, method=\"",meth,"\")", collapse = "", sep = "")
			
		} else if(index == 6) {
			
			if(methodRobust$getActive()) meth = "robust"
			
			else meth = "standard"
			
			siglevel = significanceLevel$getText()
			
			numsim = numberSimulations$getText()
			
			if(nchar(siglevel)>0) siglevel = paste(", alpha=", siglevel, collapse = "", sep = "");
			
			if(nchar(numsim)>0) numsim = paste(", R=", numsim, collapse = "", sep = "");
			
			str[3] = paste("result = adtestWrapper(x", siglevel, numsim,", robustEst=\"", meth, "\")", collapse = "", sep = "");
			
		}
		
		
		
		if(str[3] == "") return(0)
		
		
		
		resstr = "result"
		
		graph = FALSE
		
		if(index == 1) {
			
			if(variant == 0) {
				
				if(type == 0) resstr = "result$x.alr"
				
				else resstr = "result$varx"
				
			} else if(variant == 1) {
				
				if(type == 0) resstr = "result$x.clr"
				
				else resstr = "result$gm"
				
			}
			
		} else if(index == 4) {
			
			if(type == 0) resstr = "result$mahalDist"
			
			else if(type == 1) resstr = "result$outlierIndex"
			
			else graph = TRUE
			
		} else if(index == 5) {
			
			if(type == 0) resstr = "result$scores"
			
			else if(type == 1) resstr = "result$loadings"
			
			else if(type == 2) resstr = "result$eigenvalues"
			
			else graph = TRUE
			
		} else if(index == 6) {
			
			if(type == 0) resstr = "summary(result)"
			
		}
		
		
		
		if(graph) str[4] = paste("plot(",resstr,")", collapse = "", sep = "")
		
		else if(outputFileRadio$getActive() == TRUE) str[4] = paste("write.table(",resstr,", \"", outputFile$getText(), "\")", collapse = "", sep="")
		
		else if(outputVariableRadio$getActive() == TRUE) str[4] = paste("assign(\"", outputVariable$getText(), "\", ",resstr,", 1)", collapse = "", sep="")
		
		else if(outputConsoleRadio$getActive() == TRUE) {
			
			if(index == 6) str[4] = resstr
			
			else str[4] = paste("print(",resstr,")", collapse="", sep="")
			
		}
		
		
		
		out = ""
		
		for(cur in str) if(cur != "") out = paste(out, cur, "\n", collapse = "", sep = "")
		
		outputCodeView$getBuffer()$setText(out)
		
	}
	
	
	
	# signal mapping for code update
	
	changeable = c(composition, compositionVariant, constSum, ivar, quantile, inputFile, inputVariable, input2File, input2Variable, outputFile, outputVariable, outputTypeCombo, significanceLevel, numberSimulations)
	
	changeableToggle = c(methodRobust, methodStandard, inputFileRadio, inputVariableRadio, input2FileRadio, input2VariableRadio, outputFileRadio, outputVariableRadio, outputConsoleRadio)
	
	for(object in changeable) gSignalConnect(object, "changed", updateCode)
	
	for(object in changeableToggle) gSignalConnect(object, "toggled", updateCode)
	
	
	
	# run button event
	
	run <- function(d) {
		
		input = NULL
		
		input2 = NULL
		
		result = NULL
		
		
		
		# input data loading
		
		if(inputFileRadio$getActive() == TRUE) {
			
			if(nchar(inputFile$getText()) < 1) return(error("You must specify input file"))
			
			input <- try(read.table(inputFile$getText()))
			
			if(class(input) == "try-error") return(error("Error reading input file"))
			
		} else if(inputVariableRadio$getActive() == TRUE) {
			
			var = inputVariable$getActiveText()
			
			
			
			if(nchar(var) < 1) return(error("You must choose a variable for input"))
			
			if(!exists(var)) try(data(list=var))
			
			input <- try(get(var))
			
			if(class(input) == "try-error") return(error("Given variable for input not accessible"))
			
		} else {
			
			return(error("Input not specified"))
			
		}
		
		
		
		index <- composition$getActive()
		
		variant = compositionVariant$getActive()
		
		
		
		# second input loading
		
		if(index == 3) {
			
			if(input2FileRadio$getActive() == TRUE) {
				
				if(nchar(input2File$getText()) < 1) return(error("You must specify second input file"))
				
				input2 <- try(read.table(input2File$getText()))
				
				if(class(input2) == "try-error") return(error("Error reading second input file"))
				
			} else if(input2VariableRadio$getActive() == TRUE) {
				
				if(length(input2Variable$getActiveText()) < 1) return(error("You must choose a variable for second input"))
				
				try(data(input2Variable$getActiveText()))
				
				input2 <- try(get(input2Variable$getActiveText()))
				
				if(class(input2) == "try-error") return(error("Given variable for second input not accessible"))
				
			} else {
				
				return(error("second input not specified"))
				
			}
			
		}
		
		
		
		# performs computation
		
		if(index == 0) {
			
			constant = constSum$getText()
			
			if(nchar(constant)<1) result=try(constSum(input))
			
			else result = try(constSum(input, as.numeric(constant)))
			
		} else if(index == 1) {
			
			if(variant == 0) {
				
				indexVar = ivar$getText()
				
				if(nchar(indexVar)<1) result = try(addLR(input))
				
				else result = try(addLR(input, as.numeric(indexVar)))
				
			}
			
			else if(variant == 1) result = try(cenLR(input))
			
			else if(variant == 2) result = try(isomLR(input))
			
			else return(error("Variant not specified"))
			
		} else if(index == 2) {
			
			if(variant == 0) {
				
				indexVar = ivar$getText()
				
				if(nchar(indexVar)<1) result = try(addLRinv(input))
				
				else result = try(addLR(input, as.numeric(indexVar)))
				
			}
			
			else if(variant == 1) result = try(cenLRinv(input))
			
			else if(variant == 2) result = try(isomLRinv(input))
			
			else return(error("Variant not specified"))
			
		} else if(index == 3) {
			
			result = try(aDist(input, input2))
			
		} else if(index == 4) {
			
			quant = quantile$getText()
			
			if(methodRobust$getActive()) meth = "robust"
			
			else meth = "standard"
			
			if(nchar(quant)>0) result = try(outCoDa(input, quantile=as.numeric(quant), method=meth))
			
			else result = try(outCoDa(input, method=meth))
			
		} else if(index == 5) {
			
			if(methodRobust$getActive()) meth = "robust"
			
			else meth = "standard"
			
			result = try(pcaCoDa(input, method=meth))
			
		} else if(index == 6) {
			
			if(methodRobust$getActive()) meth = "robust"
			
			else meth = "standard"
			
			siglevel = significanceLevel$getText()
			
			numsim = numberSimulations$getText()
			
			if(nchar(siglevel)==0) siglevel = 0.01
			
			if(nchar(numsim)==0) numsim = 1000
			
			result = try(adtestWrapper(input, alpha = as.numeric(siglevel), R = as.numeric(numsim), robustEst = meth))
			
		} else return(error("Composition not specified"))
		
		
		
		if(class(result) == "try-error") return(error(result))
		
		
		
		type = outputTypeCombo$getActive()
		
		if(type<0) return(error("You must specify what do you want to output"))
		
		graph = FALSE
		
		
		
		# output data type specs
		
		if(index == 1) {
			
			if(variant == 0) {
				
				if(type == 0) result = result$x.alr
				
				else result = result$varx
				
			} else if(variant == 1) {
				
				if(type == 0) result = result$x.clr
				
				else result = result$gm
				
			}
			
		} else if(index == 4) {
			
			if(type == 0) result = result$mahalDist
			
			else if(type == 1) result = result$outlierIndex
			
			else graph = TRUE
			
		} else if(index == 5) {
			
			if(type == 0) result = result$scores
			
			else if(type == 1) result = result$loadings
			
			else if(type == 2) result = result$eigenvalues
			
			else graph = TRUE
			
		} else if(index == 6) {
			
			if(type == 0) result = summary(result)
			
		}
		
		
		
		# output
		
		if(graph) plot(result)
		
		else if(outputFileRadio$getActive() == TRUE) {
			
			outputFileName = outputFile$getText()
			
			if(nchar(outputFileName)<1) return(error("You must specify output file"))
			
			res <- try(write.table(result, outputFileName))
			
			if(class(res) == "try-error") return(error("Error writing into file"))
			
			else print("Data written into file")
			
		} else if(outputVariableRadio$getActive() == TRUE) {
			
			outputVariableName = outputVariable$getText()
			
			if(nchar(outputVariableName)<1) return(error("You must specify output variable"))
			
			res <- try(assign(outputVariableName, result, envir=robGUIenv()))
			
			if(class(res) == "try-error") return(error("Cannot write data into given variable"))
			
			else print("Data written into variable")
			
		} else if(outputConsoleRadio$getActive() == TRUE) {
			
			if(index == 6) T
			
			else print(result)
			
		} else {
			
			return(error("Output not specified"))
			
		}
		
	}
	
	gSignalConnect(runButton, "clicked", run)
	
	
	
	# deactivation of outputs according to selected type of output
	
	outputTypeChange = function(d) {
		
		sens = ((outputTypeCombo$getActive() != 2 || composition$getActive() != 4) && (composition$getActive() != 5 || outputTypeCombo$getActive() != 3))
		
		outputFileRadio$setSensitive(sens)
		
		outputVariableRadio$setSensitive(sens)
		
		outputConsoleRadio$setSensitive(sens)
		
		outputFile$setSensitive(sens)
		
		outputVariable$setSensitive(sens)
		
		outputBrowse$setSensitive(sens)
		
	}
	
	gSignalConnect(outputTypeCombo, "changed", outputTypeChange)
	
	
	
	# returns list of data frames currently available
	
	getLists = function() {
		
		globals = c()
		
		for(name in objects(".GlobalEnv")) {
			
			obj = get(name)
			
			if(is.matrix(obj) || is.data.frame(obj) || is.vector(obj)) globals = c(globals, name)
			
		}
		
		
		
		labels = unique(sort(c(globals, data()[3]$results[,3])))
		
		labels
		
	}
	
	
	
	# reloads var list on combo popup
	
	combovars = c()
	
	combovars2 = c()
	
	varcombo = function(d) {
		
		labels = getLists()
		
		
		
		if(!identical(combovars, labels)) {
			
			for(i in 0:length(combovars)) inputVariable$removeText(0)
			
			for(label in labels) inputVariable$appendText(label)
			
			combovars <<- labels
			
		}
		
	}
	
	varcombo2 = function(d) {
		
		labels = getLists()
		
		
		
		if(!identical(combovars2, labels)) {
			
			for(i in 0:length(combovars2)) input2Variable$removeText(0)
			
			for(label in labels) input2Variable$appendText(label)
			
			combovars2 <<- labels
			
		}
		
	}
	
	gSignalConnect(inputVariable, "popup", varcombo)
	
	gSignalConnect(input2Variable, "popup", varcombo2)
	
	
	
	# configuration management (save / load)
	
	menubar = gtkMenuBarNew()
	
	menuConfigItem <- gtkMenuItemNewWithLabel("Configuration")
	
	menuConfig <- gtkMenuNew();
	
	menuSaveItem <- gtkMenuItemNewWithLabel("save")
	
	menuLoadItem <- gtkMenuItemNewWithLabel("load")
	
	
	
	menubar$append(menuConfigItem)
	
	menuConfigItem$setSubmenu(menuConfig)
	
	menuConfig$append(menuSaveItem)
	
	menuConfig$append(menuLoadItem)
	
	
	
	saveConfig = function() {
		
		methodRadio = if(methodRobust$getActive()) "robust" else "standard"
		
		inputRadio = if(inputFileRadio$getActive()) "file" else "variable"
		
		input2Radio = if(input2FileRadio$getActive()) "file" else "variable"
		
		outputRadio = if(outputFileRadio$getActive()) "file" else
				
				if(outputVariableRadio$getActive()) "variable" else "console"
		
		
		
		c("robGUI configuration 002", nonnull(composition$getActiveText()), nonnull(compositionVariant$getActiveText()), constSum$getText(), ivar$getText(), quantile$getText(), methodRadio,
				
				inputRadio, inputFile$getText(), nonnull(inputVariable$getActiveText()), input2Radio, input2File$getText(), nonnull(input2Variable$getActiveText()),
				
				outputRadio, outputFile$getText(), outputVariable$getText(), nonnull(outputTypeCombo$getActiveText()), significanceLevel$getText(), numberSimulations$getText())
		
	}
	
	
	
	loadConfig = function(config) {
		
		config = config[,1]
		
		if(config[1] != "robGUI configuration 002") return(error("this is not a valid robGUI config file"))
		
		
		
		setActiveText(composition, config[2])
		
		setActiveText(compositionVariant, config[3])
		
		constSum$setText(config[4])
		
		ivar$setText(config[5])
		
		quantile$setText(config[6])
		
		
		
		methodRobust$setActive(config[7] == "robust")
		
		methodStandard$setActive(config[7] == "standard")
		
		inputFileRadio$setActive(config[8] == "file")
		
		inputVariableRadio$setActive(config[8] == "variable")
		
		inputFile$setText(config[9])
		
		inputVariable$getChild()$setText(config[10])
		
		input2FileRadio$setActive(config[11] == "file")
		
		input2VariableRadio$setActive(config[11] == "variable")
		
		input2File$setText(config[12])
		
		input2Variable$getChild()$setText(config[13])
		
		
		
		outputFileRadio$setActive(config[14] == "file")
		
		outputVariableRadio$setActive(config[14] == "variable")
		
		outputConsoleRadio$setActive(config[14] == "console")
		
		
		
		outputFile$setText(config[15])
		
		outputVariable$setText(config[16])
		
		
		
		compositionChanged(NULL)
		
		
		
		setActiveText(outputTypeCombo, config[17])
		
		
		
		significanceLevel$setText(config[18]);
		
		numberSimulations$setText(config[19]);
		
	}
	
	
	
	# signals of clicking the menu, writes or loads config vector to and from the chosen file
	
	saveClicked = function(d) {
		
		dialog <- gtkFileChooserDialogNew("Select output configuration file", NULL, "save",
				
				"gtk-cancel", GtkResponseType["cancel"],
				
				"gtk-open", GtkResponseType["accept"])
		
		if (dialog$run() == GtkResponseType["accept"]) {
			
			result = try(write.table(saveConfig(),dialog$getFilename()))
			
			if(class(result) == "try-error") error(result)
			
		}
		
		dialog$destroy()
		
	}
	
	gSignalConnect(menuSaveItem, "activate", saveClicked)
	
	
	
	loadClicked = function(d) {
		
		dialog <- gtkFileChooserDialogNew("Select input configuration file", NULL, "open",
				
				"gtk-cancel", GtkResponseType["cancel"],
				
				"gtk-open", GtkResponseType["accept"])
		
		if (dialog$run() == GtkResponseType["accept"]) {
			
			result = try(read.table(dialog$getFilename()))
			
			if(class(result) == "try-error") error(result)
			
			else loadConfig(result)
			
		}
		
		dialog$destroy()
		
	}
	
	gSignalConnect(menuLoadItem, "activate", loadClicked)
	
	
	
	# on app load some preactions
	
	compositionChanged(NULL)
	
	varcombo()
	
	varcombo2()
	
	
	
	# show all
	
	allBox = boxObject("v", F, 0, c(menubar, compositionFrame, inputFrame, input2Frame, outputFrame, outputCode, buttonAlign), c(F,F,F,F,F,T,F), c(F,F,F,F,F,T,F), c(0,0,0,0,0,0,0))
	
	window$add(allBox)
	
	
	
	window$setDefaultSize(400,600)
	
	window$show()
	
	window$move(window$getScreen()$getWidth()-400,0)
	
}



