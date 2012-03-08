/*
 *    JdeUtilities.java
 *    $Revision: 1.6 $
 *
 *    Copyright (C) 1999-2001 Len Trigg (trigg@cs.waikato.ac.nz)
 *    Copyright (C) 1999-2001 Paul Kinnucan (paulk@mathworks.com)
 *
 *    This program is free software; you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation; either version 2 of the License, or
 *    (at your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software
 *    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

package jde.util;

import java.io.File;
import java.io.FilenameFilter;
import java.util.Enumeration;
import java.util.StringTokenizer;
import java.util.Vector;
import java.util.Hashtable;
import java.util.zip.ZipFile;
import java.util.zip.ZipEntry;


/**
 * This class provides various utility methods.
 *
 * @author Len Trigg (trigg@cs.waikato.ac.nz) 
 * @author Paul Kinnucan (paulk@mathworks.com)
 * @author Matt Conway (Matt_Conway@i2.com )
 * @version $Revision: 1.6 $
 */
public class JdeUtilities {

  /** A cache of the items that are important across projects, indexed by the project name */
  private static Hashtable projectCache = new Hashtable();

  /** The current project so that callers need not pass in the
      project name every time.  This is convenient, but not
      threadsafe.  If we need thread saftey, we can go change all the
      emacs lisp callers to always pass in the project name */
  private static String currentProjectName = "default";
	
  // Have a default one just in case
  static {
    projectCache.put(currentProjectName, new CacheEntry(System.getProperty("java.class.path")));
  }
		
  /* A Private class for storing multiple items in a single hashtable
     entry.  This is easily extensible if we need to add more project
     specific data later on */
  private static class CacheEntry
  {
    /* The list of all the classes in classPath for the project */
    public Vector classList;
    /* The classpath for the project.  The classlist is generated
       from this */
    public String classPath;
    /* We are dirty if classList is older than classPath */
    public boolean dirty;

    CacheEntry(String classPath)
    {
      this.classList = new Vector(500);
      this.classPath = classPath;
      this.dirty = true;
    }
  }

  /* Jde should call this everytime the project changes, or if the
     classpath needs to be updated */
  public static void setProjectValues(String projectName, String projectClassPath)
  {
    currentProjectName = projectName;
    CacheEntry ce = (CacheEntry) projectCache.get(projectName);
    if (ce == null)
      {
	ce = new CacheEntry(projectClassPath);
	projectCache.put(projectName, ce);
      } 
    else if (! ce.classPath.equals(projectClassPath))
      {
	ce.classPath = projectClassPath;
	ce.dirty = true;
      }	
  }

  /* Convenience to get current project's name */
  public static String getCurrentProjectName()
  {
    return JdeUtilities.currentProjectName;
  }

  /* Convenience to get current project's classpath */
  public static String getCurrentClassPath()
  {
    return getClassPath(currentProjectName);
  }

  /* Gets the classpath for projectName */
  public static String getClassPath(String projectName)
  {
    CacheEntry ce = (CacheEntry) projectCache.get(projectName);
    return ce.classPath;
  }

  /* Convenience to get current project's classlist */
  public static Vector getCurrentClassList()
  {
    return getClassList(currentProjectName);
  }

  /* Gets the classlist for projectName.  If the classList is older
     than the classPath, or empty, it gets regenerated */
  public static Vector getClassList(String projectName)
  {
    CacheEntry ce = (CacheEntry) projectCache.get(projectName);
    buildClassList(false);
    return ce.classList;
  }
			
  public static void classExists( String fqn ) {
    try {
      Class.forName( fqn );
      System.out.println( "t" );
    } catch (NoClassDefFoundError ex1) {
      System.out.println( "nil" );
    } catch(Exception ex2) {
      System.out.println( "nil" );
    }//catch
  }//met

  /* Lazily generates the classlist for the current project depending
     on its dirty status.  If force is true, then always gets
     generated */
  public static void buildClassList(boolean force) {
    String classPathSeparator = File.pathSeparator;

    CacheEntry ce = (CacheEntry) projectCache.get(currentProjectName);

    String classPath = ce.classPath;
    Vector classList = ce.classList;
    boolean dirty = ce.dirty;

    if (force || dirty || classList.size() == 0)
      {
	// clean up path list first
	classList.setSize(0);

	// For Java 2!
	String classPath2 = System.getProperty("sun.boot.class.path");
	if (classPath2 != null)
	  classPath += classPathSeparator + classPath2;
		 
	// Add all files from the extension directories to the classPath
	String extDirs = System.getProperty("java.ext.dirs");
	if (extDirs != null) {
	  StringTokenizer st = new StringTokenizer(extDirs, classPathSeparator);
			 
				// Iterate through extension directories
	  while (st.hasMoreTokens()) {
	    File[] extFiles = new File(st.nextToken()).listFiles();
				 
	    if (extFiles != null) {
	      // Iterate through files added them to classPath
	      for (int i = 0; i < extFiles.length; i++) {
		classPath += classPathSeparator + extFiles[i].getPath();
	      }
	    }
	  }
	}
		 
	// System.out.println("classpath = " + classPath);
		 
	StringTokenizer st = new StringTokenizer(classPath, classPathSeparator);
	while (st.hasMoreTokens()) {
	  String classPathEntry = st.nextToken();
	  File classPathFile = new File(classPathEntry);
	  if (classPathFile.exists()) {
	    if (classPathEntry.toLowerCase().endsWith(".jar")) {
	      addClassesFromZip(classList, classPathFile);
	    } else if (classPathEntry.toLowerCase().endsWith(".zip")) {
	      addClassesFromZip(classList, classPathFile);
	    } else if (classPathFile.isDirectory()) {
	      addClassesFromDir(classList, classPathFile, classPathFile);
	    }
	  }
	}
	ce.dirty = false;
      }
  }
    
  
  /**
   * Adds the classes from the supplied Zip file to the class list.
   *
   * @param classList the Vector to add the classes to
   * @param classPathFile the File to scan as a zip file
   */
  public static void addClassesFromZip(Vector classList,
				       File classPathFile) {
    // System.out.println("Processing jar/zip file: " + classPathFile);
    
    try {
      ZipFile zipFile = new ZipFile(classPathFile);
      Enumeration enum = zipFile.entries();
      while (enum.hasMoreElements()) {
	ZipEntry zipEntry = (ZipEntry)enum.nextElement();
	String current = zipEntry.getName();
	if (current.toLowerCase().endsWith(".class")) {
	  current = current.substring(0, current.length() - 6);
	  current = current.replace('/', '.');
	  current = current.replace('\\', '.');
	  classList.addElement(current);
	}
      }
    } catch (Exception ex) {
      System.err.println("Problem opening " + classPathFile + " with zip.");
    }
  }

  
  /**
   * Adds the classes from the supplied directory to the class list.
   *
   * @param classList the Vector to add the classes to
   * @param classPathFile the File to recursively scan as a directory
   */
  public static void addClassesFromDir(Vector classList,
				       File rootDir,
				       File currentDir) {
    
    String [] files = currentDir.list();

    if (files == null) {
      System.err.println("Cannot read contents of " + currentDir + ".");
      return;
    } // end of if ()
    
    for (int i = 0; i < files.length; i++) {
      String current = files[i];
      if (current.toLowerCase().endsWith(".class")) {
	current = current.substring(0, current.length() - 6);
	String rootPath = rootDir.getPath();
	String currentPath = currentDir.getPath();
	if (currentPath.indexOf(rootPath) != 0) {
	  System.err.println("currentPath doesn't start with rootPath!\n"
			     + "rootPath: " + rootPath + "\n"
			     + "currentPath: " + currentPath + "\n");
	} else {
	  String packageName = currentPath.substring(rootPath.length());
	  if (packageName.length() > 0) {
	    // Not the current directory
	    packageName = packageName.replace('\\', '.');
	    packageName = packageName.replace('/', '.');
	    classList.addElement(packageName.substring(1) + '.' + current);
	  } else {
	    // The current directory
	    classList.addElement(current);
	  }
	}
      } else {
	// Check if it's a directory to recurse into
	File currentFile = new File(currentDir, current);
	if (currentFile.isDirectory()) {
	  addClassesFromDir(classList, rootDir, currentFile);
	}
      }
    }
  }
  
  /**
   * Looks up an unqualified class name in the class path to find possible
   * fully qualified matches.
   *
   * @param className a value of type 'String'
   */
  public static void getQualifiedName(String className) {

    String qualifiedNameList = "(list";
    Vector classList = getCurrentClassList();
    for (int i = 0; i < classList.size(); i++) {
      String testName = (String) classList.elementAt(i);
			
      if ((testName.length() > className.length() && testName.endsWith(className) &&
	   testName.charAt(testName.length() - className.length() - 1) == '.') ||
	  (testName.length() == className.length()) && testName.equals(className)) {
	
	// Avoid duplicates!
	testName = " \"" +  testName + "\"";
	if (qualifiedNameList.indexOf(testName) == -1)
          qualifiedNameList += testName;
      }
    }

    qualifiedNameList += ")";
    

    System.out.println(qualifiedNameList);
    System.out.flush();

  }


} // JdeUtilities

/*
 * $Log: JdeUtilities.java,v $
 * Revision 1.6  2001/05/31 05:08:44  paulk
 * Adds caching of project data. Thanks to Matt Conway.
 *
 * Revision 1.5  2000/12/19 04:16:38  paulk
 * Now handles case where a directory is unreadable. Thanks to Michael Duvigneau <5duvigne@informatik.uni-hamburg.de> for diagnosing this error.
 *
 * Revision 1.4  2000/11/17 03:54:41  paulk
 * Adds a version of buildClassList that takes a classpath. Thanks to Raffael Herzog <herzog@raffael.ch>.
 *
 * Revision 1.3  2000/10/20 04:20:03  paulk
 * *** empty log message ***
 *
 * Revision 1.2  2000/06/22 02:27:25  paulk
 * Now adds Java extension classes to the class list. Thanks to Mark Gibson <mark@markg.co.uk>
 * for this enhancement.
 *
 * Revision 1.1.1.1  1999/08/20 01:19:51  paulk
 * Imported source
 *
 *
 */

// End of JdeUtilities.java
