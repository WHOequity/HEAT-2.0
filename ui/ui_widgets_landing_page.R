# © Copyright World Health Organization (WHO) 2016.
# This file is part of the Health Equity Assessment Toolkit (HEAT).
# HEAT is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License Version 2 as published by
# the Free Software Foundation.
# 
# HEAT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with HEAT. If not, see http://www.gnu.org/licenses/.

# These are the widgets that get added to the landing page.
# They are added via an htmlTemplate (landing_page_upload.html) that
# is reference in ui.R


# Use chooses if they want to choose a new or previously calculated dataset
ui_landing_neworexisting <- radioButtons("neworexisting", label = "", 
                                    choices = c("Create new data" = "newdata", 
                                                "Use previously calculated data" = "existdata"),
                                    inline = FALSE)


# If user chooses savesavenew then we need all of these widgets

# This is using HTML to get rid of the progress indicator
# ui_landing_upload_sub <- HTML('
# 		<p><strong>Select database</strong>&nbsp;
# 			<span class="glyphicon glyphicon-info-sign tooltip-upload"
# 				data-toggle="tooltip" data-original-title="Databases require a specific format in order to be uploaded to HEAT Plus. The HEAT Plus template exemplifies the required structure, variables, order, etc. Please refer to the HEAT Plus user manual for further information on specific requirements and instructions on how to prepare databases for use in HEAT Plus.">
# 			</span>
# 		</p>
# 		<div>
# 			<div class="fileUpload btn btn-primary btn-sm">
# 				<span>Browse...</span>
# 					<input class = "upload" id="filename" name="filename" type="file" accept="text/csv,text/comma-separated-values,text/plain,.csv, .xls, .xlsx"/>
# 			</div>
# 			<span class="browse-text"></span>
# 			<p class="italic">Download the <a href="HEATplusTemplate.xlsx" target="_blank">template</a> and <a href="http://apps.who.int/gho/data/node.main.HE-1540?lang=en" target="_blank">user manual</a>.
# 			</p> 
# 		</div><br><br>')


ui_landing_upload_sub <- HTML(		'<p><strong>Select database</strong>&nbsp;
			<span class="glyphicon glyphicon-info-sign tooltip-upload"
				data-toggle="tooltip" data-original-title="Databases require a specific format in order to be uploaded to HEAT Plus. The HEAT Plus template exemplifies the required structure, variables, order, etc. Please refer to the HEAT Plus user manual for further information on specific requirements and instructions on how to prepare databases for use in HEAT Plus.">
			</span>
		</p>

<div class="form-group shiny-input-container">
  <div class="input-group">
    <label class="input-group-btn">
      <span class="btn btn-default btn-file">
        Browse...
<form>
        <input id="filename" name="filename" type="file" style="display: none;"/>
</form>
      </span>
    </label>
    <input id = "filenametxt" type="text" class="form-control" placeholder="No file selected" readonly="readonly"/>
  </div>
  <div id="filename_progress" class="progress progress-striped active shiny-file-input-progress">
    <div class="progress-bar"></div>
  </div>
</div>
			<p class="italic">Download the <a href="HEATplusTemplate.xlsx" target="_blank">template</a> and <a href="User_manual.pdf" target="_blank">user manual</a>.
			</p> <br><br>')


#ui_landing_upload_sub <- fileInput("filename", "")



ui_landing_upload_foldersel_sub <- textInput("datafoldername", HTML("<p>Save database as&nbsp;
									<span class='glyphicon glyphicon-info-sign tooltip-upload'
									data-toggle='tooltip' data-original-title='A name has to be provided for the uploaded database. The database with the disaggregated data and calculated summary measures will be stored under this name. Note that if the name already exists, the existing database will be overwritten. Please refer to the user manual for information on how to delete existing databases.'></span></p>"),
                                   gsub(" |-|:", "", paste0("data", Sys.Date())))

# ui_landing_radioBS_sub <- conditionalPanel("input.neworexisting == 'newdata'",
#   radioButtons("doBS", label = "Compute bootstrap standard error", 
#                                    choices = c("No", "Yes"), selected = "No", inline = TRUE)
# )

ui_landing_uploadnew_list <- list(ui_landing_upload_sub, 
                             #ui_landing_filetype_sub, 
                             #ui_landing_sheetnum_sub,
                             ui_landing_upload_foldersel_sub)#,
                             #ui_landing_radioBS_sub)

# If user chooses previously calculated then we need these widgets

ui_landing_uploadnew <- conditionalPanel("input.neworexisting == 'newdata'",
                 ui_landing_uploadnew_list)


ui_landing_foldersel_sub <- selectInput("selectfolders", "Select database", choices = get_data_folders())
ui_landing_foldersel <- conditionalPanel("input.neworexisting == 'existdata'",
                 ui_landing_foldersel_sub)



#testM-conditionalPanel()




