/**
 * @author Melanie
 * Feb 25, 2008
 * 
 * Filename    : OBIReleaseClient.java
 * Copyright (C)  Melanie Courtot 2008
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more
 * details. http://www.gnu.org/licenses/gpl.txt
 * Melanie Courtot
 * Terry Fox Laboratory, BC Cancer Research Centre. Vancouver, BC V5Z 1L3  Canada. mcourtot@bccrc.ca
 */

import ca.bccrc.obi.*;


public class OBIReleaseClient {
    public static void main(String [] args)
    {
 	
    	OBIMerger mo = new OBIMerger();
    
 	 
    	 
    	//TO MODIFY ==================================================================
   
		
    	//we need to check that we got the 3 needed arguments
    	if (args.length != 3){
    		System.out.println ("You need to pass the correct arguments, first the path of the file that will be created,i.e. the merged file, second the path of the protege firendly file and third the physical URI of the files.");
    		System.out.println("A correct call would be for example java OBIReleaseClient \"/Users/melanie/Desktop/JenaMerge.owl\" \"/Users/melanie/Desktop/OBI/SVN/obi/\" ");
    	}
    	
    	else
    	{
    		
    	 	//the file that will be created,i.e. the merged file
     		//String newFilePath = "/Users/melanie/Desktop/JenaMerge.owl";
        	String newFilePath = args[0];
    		//the physical URI of the files
    		
        	
        	//String newFilePathProtegeFriendly = "/Users/melanie/Desktop/FINAL_MERGE_PROTEGE_FRIENDLY.owl";
        	String newFilePathProtegeFriendly = args[1];
        	
        	//String physicalURI = "/Users/melanie/Desktop/OBI/SVN/obi/";
        	String physicalURI = args[2];
	
		// ===========================================================================
		mo.mergeFiles(newFilePath,physicalURI,false);
		//check consistency
		boolean valid = mo.checkConsistency(newFilePath);
	
		if (valid){
			System.out.println("true");
		}
		else {
			System.out.println("false");
		}
	
		
		mo.mergeFiles(newFilePathProtegeFriendly,physicalURI,true);
		
		
    	}
    	
    	
    	
    	
		

    }


}



