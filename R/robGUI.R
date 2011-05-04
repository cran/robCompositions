#library(robCompositions)
#library(RGtk2)

robGUI = function() {
	is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
	if(!is.installed('RGtk2')) warning("RGtk2 have to be installed first.") else cat("\n ... the package RGtk2 has to be loaded.\n")
	
	appendTexts = function(cont, texts) {
		for(text in texts) cont$appendText(text)
	}
	
	boxObject = function(type, homogenous, spacing, children, expand, fill, padding) {
	  if(type == "h")	box = gtkHBoxNew(homogenous, spacing)
	  else box = gtkVBoxNew(homogenous,spacing)
	  i = 1
	  for(child in children) {
			box$packStart(child, expand[i], fill[i], padding[i])
			i = i+1
		}
	  box
	}
	
	align = function(object, xalign, yalign, xscale, yscale) {
		alignment = gtkAlignmentNew(xalign, yalign, xscale, yscale)
		alignment$add(object)
		alignment
	}
	
	padding = function(object, top, bottom, left, right) {
	  alignment = gtkAlignmentNew()
	  alignment$add(object)
	  alignment$setPadding(top, bottom, left, right)
	  alignment
	}
	
	nonnull = function(value) { if(length(value)<1) return("") else return(value) }
	
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
	
	setShown = function(object, show) {
	  if(show) object$show()
	  else object$hide()
	}
	
	helps = c("Choose composition",
						"Closes compositions to sum up to a given constant (default 1),\nby dividing each part of a composition by its row sum.",
            "The transformation moves D-part compositional data\nfrom the simplex into a (D-1)-dimensional real space.",
	          "Inverse log-ratio transformation,\noften called logistic transformation.",
	          "Computes the Aitchison distance\nbetween two observations or between two data sets.",
	          "Outlier detection for compositional data\nusing standard and robust statistical methods.",
						"The compositional data set is transformed using the ilr\ntranformation.  Afterwards, robust principal component analysis is\nperformed.  Resulting loadings and scores are back-transformed to\nthe clr space where  the compositional biplot can be shown."
						)
	
	# construction
	window = gtkWindowNew(NULL, FALSE)
	window$setTitle("robCompositions GUI")
	window$setDefaultSize(400,400)

	# composition frame
	compositionFrame = gtkFrameNew("Composition")
	compositionFrame$setBorderWidth(5)

	composition = gtkComboBoxNewText()
	composition$show()
	appendTexts(composition, c("Closure operation", "Logratio transformation", "Inverse logration transformation", "Aitchison distance", "Outlier detection", "Robust principal component analysis"))
	
	compositionVariant = gtkComboBoxNewText()
	compositionVariant$show()
	appendTexts(compositionVariant, c("additive", "centered", "isometric"))
	compositionVariantBox = boxObject("h", F, 0, c(padding(gtkLabelNew("variant:"),0,0,10,5), compositionVariant), c(T,T), c(T,T), c(0,0))
	
	compositionHelp = gtkLabelNew("napoveda")
	compositionSelect = boxObject("h", F, 0, c(align(composition,0,0,1,0), align(compositionVariantBox,1,0,0,0)), c(T,F), c(T,F), c(0,0))
	
	constSum = gtkEntryNew()
	compositionConstSum = boxObject("h", F, 0, c(padding(gtkLabelNew("closure constant:"),0,0,0,5), constSum), c(F,F), c(F,T), c(0,0))
	
	ivar = gtkEntryNew()
	compositionIvar = boxObject("h", F, 0, c(padding(gtkLabelNew("index of the rationing part:"),0,0,0,5), ivar), c(F,T), c(F,T), c(0,0))
	
	quantile = gtkEntryNew()
	compositionQuantile = boxObject("h", F, 0, c(padding(gtkLabelNew("quantile:"),0,0,0,5), quantile), c(F,F), c(F,T), c(0,0))
	
	methodRobust = gtkRadioButtonNewWithLabel(NULL, "robust")
	methodStandard = gtkRadioButtonNewWithLabel(methodRobust$getGroup(), "standard")
	methodBox = boxObject("h", F, 0, c(gtkLabelNew("method:"), methodRobust, methodStandard), c(F,T,T), c(F,F,F), c(0,0,0))
	
	compositionBox = boxObject("v", F, 0, c(compositionSelect, padding(compositionConstSum,10,0,0,0), padding(compositionIvar,10,0,0,0), padding(compositionQuantile,10,0,0,0), methodBox, align(padding(compositionHelp,10,0,0,0),0,0,0,0)), c(F,T,T,T,T,F), c(F,T,T,T,T,F), c(0,0,0,0,0,0))
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
	
	#auto switch radio
  gSignalConnect(inputFile, "changed", function(d) {inputFileRadio$setActive(TRUE)})
  gSignalConnect(inputVariable, "popup", function(d) {inputVariableRadio$setActive(TRUE)})
  gSignalConnect(input2File, "changed", function(d) {input2FileRadio$setActive(TRUE)})
  gSignalConnect(input2Variable, "popup", function(d) {input2VariableRadio$setActive(TRUE)})
  gSignalConnect(outputFile, "changed", function(d) {outputFileRadio$setActive(TRUE)})
  gSignalConnect(outputVariable, "changed", function(d) {outputVariableRadio$setActive(TRUE)})

	# run button
	runButton = gtkButtonNewWithLabel("Run")
	buttonAlign = align(runButton,0,1,1,0)
	
	# browse button logic
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
	
	# hiding composition logic
	compositionChanged = function(d) {
		index = composition$getActive()
		variantIndex = compositionVariant$getActive()
		
		setShown(compositionVariantBox, index == 1 || index == 2)
		setShown(compositionConstSum, index == 0)
		setShown(compositionIvar, (index == 1 || index == 2) && variantIndex == 0)
		setShown(compositionQuantile, index == 4)
		setShown(methodBox, index == 4 || index == 5)
		setShown(input2Frame, index == 3)
		
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

		appendTexts(outputTypeCombo, append)
		if(length(append) > 0) outputTypeCombo$setActive(0)
	}
	gSignalConnect(composition, "changed", compositionChanged);
	gSignalConnect(compositionVariant, "changed", compositionChanged);
	
	# run button action, do all
	error <- function(msg) {
		dialog <- gtkMessageDialog(window, "destroy-with-parent", "error", "close", msg)
		dialog$run()
		dialog$destroy()
	}
	
	run <- function(d) {
	  input = NULL
		input2 = NULL
		result = NULL

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

		if(index == 0) {
		  constant = constSum$getText()
		  if(nchar(constant)<1) result=try(constSum(input))
			else result = try(constSum(input, as.numeric(constant)))
		} else if(index == 1) {
			if(variant == 0) {
			  indexVar = ivar$getText()
				if(nchar(indexVar)<1) result = try(alr(input))
				else result = try(alr(input, as.numeric(indexVar)))
			}
			else if(variant == 1) result = try(clr(input))
			else if(variant == 2) result = try(ilr(input))
			else return(error("Variant not specified"))
		} else if(index == 2) {
			if(variant == 0) {
			  indexVar = ivar$getText()
				if(nchar(indexVar)<1) result = try(invalr(input))
				else result = try(alr(input, as.numeric(indexVar)))
			}
			else if(variant == 1) result = try(invclr(input))
			else if(variant == 2) result = try(invilr(input))
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
		} else return(error("Composition not specified"))
		
		if(class(result) == "try-error") return(error(result))
		
		type = outputTypeCombo$getActive()
		if(type<0) return(error("You must specify what do you want to output"))
		graph = FALSE

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
		}
		
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
			res <- try(assign(outputVariableName, result, 1))
			if(class(res) == "try-error") return(error("Cannot write data into given variable"))
			else print("Data written into variable")
		} else if(outputConsoleRadio$getActive() == TRUE) {
			print(result)
		} else {
			return(error("Output not specified"))
		}
	}
  gSignalConnect(runButton, "clicked", run)
  
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
  
  getLists = function() {
    globals = c()
		for(name in objects(".GlobalEnv")) {
		  obj = get(name)
		  if(is.matrix(obj) || is.data.frame(obj) || is.vector(obj)) globals = c(globals, name)
		}

		labels = unique(sort(c(globals, data()[3]$results[,3])))
	 	labels
	}
  
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

	# configuration
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

	  c("robGUI configuration 001", nonnull(composition$getActiveText()), nonnull(compositionVariant$getActiveText()), constSum$getText(), ivar$getText(), quantile$getText(), methodRadio,
	          inputRadio, inputFile$getText(), nonnull(inputVariable$getActiveText()), input2Radio, input2File$getText(), nonnull(input2Variable$getActiveText()),
	          outputRadio, outputFile$getText(), outputVariable$getText(), nonnull(outputTypeCombo$getActiveText()))

	}

	loadConfig = function(config) {
	  config = config[,1]
	  if(config[1] != "robGUI configuration 001") return(error("this is not a valid robGUI config file"))
	
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
	}
	
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
	
	compositionChanged(NULL)
	varcombo()
	varcombo2()

	# show all
	allBox = boxObject("v", F, 0, c(menubar, compositionFrame, inputFrame, input2Frame, outputFrame, buttonAlign), c(F,F,F,F,F,T), c(F,F,F,F,F,T), c(0,0,0,0,0,0))
	allBox$setSizeRequest(400,500)
	window$add(allBox)
	
	window$show()
	window$move(window$getScreen()$getWidth()-500,100)
	window$setResizable(FALSE)
}

#robGUI()


