## Overview
Output checking is a core task for any IDI researcher submitting data for release to Stats. While the Output Checking Tool helps researchers to check outputs, it can be time consuming especially in large projects with multiple files. The Auto checker tool further accelerates this process, reducing the time researchers spend output checking. This tool is designed for repeated outputs where files share an identical file structure. 

The auto checker reuses the components from the existing Checking tool and bundles functions into a single compact script. It requires minimal user input, removing the repetitive task of manually assigning column types and links. The auto checker has been designed to be integrated into an automated pipeline process which further enhances the benefits of using the tool.

## How to use

There are 3 user parameters:

- Folder path: Path to the folder containing the CSV files
  -	Files must be provided in RAW/CONF pairs with matching names (e.g, output_RAW.csv and output_CONF.csv
  -	First row of each file needs to be the column names and the file must not include metadata or header rows
  
- Column types: A named numeric vector describing what each column is
  -	Each element in the vector must be named using the output column name and assigned a column type.
  -	The column type number corresponds to the type defined in the lookup table. Instructions for accessing the lookup table is provided.
  
- Column links: A named list that links the conf to the raw and entity columns.
  -	A list that specifies which columns of the raw file should be used to check each column of the conf file. 
  -	The lookup table can be used to confirm what inputs each column type expects.
  
Once user parameters are set, the script can be run. The tool saves two files: the interpretation structure and the checking results. The interpretation structure can be loaded into the Output Checking Tool to review how the auto checker has been configured.

For more detailed instructions and examples, refer to the header of the script. Included are two example datasets that correspond to the example user parameters in the script.

## Citation
Social Investment Agency (2025). SQL code styler. Source code. https://github.com/nz-social-investment-agency/auto_checker

## Getting Help
Enquiries can be sent to info@sia.govt.nz
