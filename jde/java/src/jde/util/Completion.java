/*
 *    Completion.java
 *    Copyright (C) 1999, 2001 Rodrigo Reyes (reyes@chez.com)
 *
 *    $Revision: 1.10 $
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

import java.lang.reflect.Modifier;
import java.lang.reflect.Field;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Member;

/**
 * This class provides completion facilities.
 *
 * @author Rodrigo Reyes (reyes@chez.com) 
 */

public class Completion {

  /**
   * Tests whether a class is an ancestor of another class.
   * This method prints "t" to standard out if the class is an ancestor
   * of the other class. If the class is not an ancestor or either class
   * cannot be found, this method prints nil to standard out.
   * @param ancestor Name of the ancestor class
   * @param child  Name of the supposed child class
   */
  public static void isAncestorOf( String ancestor, String child)
  {
    try
    {
      Class classAncestor = Class.forName( ancestor );
      Class classChild = Class.forName( child );
      if( classAncestor.isAssignableFrom( classChild ) )
        System.out.println( "t" );
      else
        System.out.println( "nil" );
    }//try
    catch( Exception ex)
    {
    System.out.println( "nil" );
    }//catch
  }//met

  /**
  *Returns true if the Member is accessible to the specified level of protection.
  *@param member the Member to check
  *@param level the level of protection. 0 means accessible to a class that inherits from the class whose
  *member is a member. 1 means accessible as a private only field.
  *@return if the Member is accessible to the specified level of protection.
  */
  private static boolean isAccessible( Member member, int level)
  {
    switch(level)
    {
      case 0: return Modifier.isProtected( member.getModifiers() );

      case 1: return ((Modifier.isPrivate(member.getModifiers())) ||
                      Modifier.isProtected(member.getModifiers()));

      default: return false;
    }//switch
  }//met

  
  /**
   *Returns true if the Class is accessible to the specified level of protection.
   *@param class the Class to check
   *@param level the level of protection. 0 means accessible to a class that 
   *inherits from the class whose class is a member. 
   *1 means accessible as a private only field.
   *@return if the Class is accessible to the specified level of protection.
   */
  private static boolean isAccessible( Class cls, int level)
  {
    switch(level)
    {
      case 0: return Modifier.isProtected( cls.getModifiers() );

      case 1: return ((Modifier.isPrivate(cls.getModifiers())) ||
                      Modifier.isProtected(cls.getModifiers()));

      default: return false;
    }//switch
  }//met
  
  private static void listClassInfo(Class c, int level) 
  {
    System.out.println("(list ");


    System.out.print("(list ");
    //We first get all the public fields
    Field[] fields = c.getFields();
    
    for (int index=0; index<fields.length; index++) {
      Field field = fields[index];
    	 System.out.print("(list \""+field.getName()+"\" \""+
			  className(field.getType())+"\")");
    }
    //we add the protected/private fields depending on the access level
    fields = c.getDeclaredFields();
    for (int index=0; index<fields.length; index++) {
      Field field = fields[index];
      if (isAccessible( field, level))
  	 System.out.print("(list \""+field.getName()+"\" \""+
			  className(field.getType())+"\")");
    }
    System.out.println(")");

	

    System.out.print("(list ");
    //we first get all the public constructors
    Constructor[] constrs = c.getConstructors();
    for (int index=0; index<constrs.length; index++) {
      Constructor constructor = constrs[index];
       System.out.print("(list \"");
	     System.out.print(constructor.getName()+"\" ");
	     listClassArray(constructor.getParameterTypes());
       System.out.print(")");
      }
    constrs = c.getDeclaredConstructors();
    for (int index=0; index<constrs.length; index++) {
      Constructor constructor = constrs[index];
      if( isAccessible( constructor,level) )
      {
        System.out.print("(list \"");
	      System.out.print(constructor.getName()+"\" ");
	      listClassArray(constructor.getParameterTypes());
        System.out.print(")");
      }
    }
    System.out.print(")");

	

    System.out.println("(list ");
    Method[] methods = c.getMethods();
    for (int index=0; index<methods.length; index++) {
      Method method = methods[index];
      System.out.print("(list \"");
      System.out.print(method.getName()+"\" \"");
      System.out.print(className(method.getReturnType()) + "\" ");
      listClassArray(method.getParameterTypes());
      System.out.print(")");
    }
    methods = c.getDeclaredMethods();
    for (int index=0; index<methods.length; index++) {
      Method method = methods[index];
      if (isAccessible( method, level)) 
      {
        System.out.print("(list \"");
        System.out.print(method.getName()+"\" \"");
        System.out.print(className(method.getReturnType()) + "\" ");
        listClassArray(method.getParameterTypes());
        System.out.print(")");
      }
    }
    System.out.println(")");
	
    //Adding inner classes
    Class[] classes = c.getDeclaredClasses();
    System.out.println("(list ");
    int length = classes.length;
    for (int i = 0; i < length; i++) {
      Class cls = classes[i];
      if (isAccessible(cls, level)) {
        System.out.print("(list \"");
        System.out.print(cls.getName() + "\"");
        System.out.print(")");
      } // end of if (Modifier.isPublic)
    } // end of for (int i = 0; i < length; i++)
    
    System.out.println(")");
    System.out.println(")");
    return;
  }
	
  private static void listClassInfo(Class c) 
  {
    System.out.println("(list ");

    Field[] fields = c.getFields();
    System.out.print("(list ");
    for (int index=0; index<fields.length; index++) {
      Field field = fields[index];
      if (Modifier.isPublic(field.getModifiers()))
  	 System.out.print("(list \""+field.getName()+"\" \""+
			  className(field.getType())+"\")");
    }
    System.out.println(")");

	
    Constructor[] constrs = c.getDeclaredConstructors();
    System.out.print("(list ");
    for (int index=0; index<constrs.length; index++) {
      Constructor constructor = constrs[index];
      if (Modifier.isPublic(constructor.getModifiers())) {
	System.out.print("(list \"");
	System.out.print(constructor.getName()+"\" ");
	listClassArray(constructor.getParameterTypes());
	System.out.print(")");
      }
      }
    System.out.print(")");

	
    Method[] methods = c.getMethods();
    System.out.println("(list ");
    for (int index=0; index<methods.length; index++) {
      Method method = methods[index];
      if (Modifier.isPublic(method.getModifiers())) {

	System.out.print("(list \"");
	System.out.print(method.getName()+"\" \"");
			
	System.out.print(className(method.getReturnType()) + "\" ");
			
	listClassArray(method.getParameterTypes());
	System.out.print(")");
      }
      }
    System.out.println(")");

    //Adding inner classes
    Class[] classes = c.getDeclaredClasses();
    System.out.println("(list ");
    int length = classes.length;
    for (int i = 0; i < length; i++) {
      Class cls = classes[i];
      if (Modifier.isPublic(cls.getModifiers())) {
        System.out.print("(list \"");
        System.out.print(cls.getName() + "\"");
        System.out.print(")");
      } // end of if (Modifier.isPublic)
    } // end of for (int i = 0; i < length; i++)
    
    System.out.println(")");
    System.out.println(")");
    return;
  }
	
  public static void getClassInfo(String className)
  {
    try {
      DynamicClassLoader dcl = new DynamicClassLoader();
      Class c = dcl.loadClass(className);
      if (c != null)
	listClassInfo(c);
    } catch (ClassNotFoundException cnfe) {System.out.println("nil");}
  }

  public static void getClassInfo(String className, int accessLevel)
  {
    try {
	    
      DynamicClassLoader dcl = new DynamicClassLoader();
      Class c = dcl.loadClass(className);
      if (c != null)
	listClassInfo(c,accessLevel);
    } catch (ClassNotFoundException cnfe) {System.out.println("nil");}
  }

  /**
   * Looks up an unqualified class name in the class path to find possible
   * fully qualified matches.
   *
   * @param className a value of type 'String'
   */
  public static void getClassInfo(String className, String[]imports, int accessLevel) 
  {
    //	System.out.println("length : " + imports.length);
    for (int i=0; i<imports.length; i++)
      {
	String name = imports[i]+className;
	try {
	  Class c = Class.forName(name);
	  if (c != null)
	    {
	      listClassInfo(c,accessLevel);
	    }
	} catch (ClassNotFoundException cnfe) { }
		
      }
    System.out.println("nil");
  }

  static String className(Class c)
  {
    if (c.isArray())
      return c.getComponentType().getName() + "[]";
    else
      return c.getName();
  }
    
  static void listClassArray(Class[] classes)
  {
    //	System.out.println("(list ");
    for (int i=0; i<classes.length; i++)
      {
	System.out.print("\"");
	System.out.print(className(classes[i]));
	System.out.print("\" ");
      }
    //	System.out.println(")");
  }

} // Completion

/*
 * $Log: Completion.java,v $
 * Revision 1.10  2001/08/14 05:15:02  paulk
 * Miscellaneous updates.
 *
 * Revision 1.9  2001/07/21 03:56:23  paulk
 * Addional inner class support. Thanks to Javier Lopez.
 *
 * Revision 1.8  2001/07/21 03:42:24  paulk
 * Now includes inner classes in the class info list. Thanks to Javier Lopez.
 *
 * Revision 1.7  2001/07/06 02:00:09  paulk
 * Makefile
 *
 * Revision 1.6  2001/06/27 03:09:04  paulk
 * The isAccessible method now returns true for both private and protected methods
 * when the access modifier is private. Thanks to "Javier Lopez" <jlopez@cellexchange.com>.
 *
 * Revision 1.5  2000/08/10 08:46:25  paulk
 * Now outputs nil if class info not found.
 *
 * Revision 1.4  2000/08/01 07:44:49  paulk
 * Adds an isAncestorOf method. Now gets private and protected methods and fields. Thanks to Stephan Nicolas.
 *
 * Revision 1.3  2000/07/27 04:49:52  paulk
 * Now returns the type as well as name of each public field of a class. Thanks to Stephane Nicolas <s.nicolas@videotron.ca>.
 *
 * Revision 1.2  2000/02/09 04:48:41  paulk
 * Now uses Modifier.isPublic() to test whether a class's fields,
 * methods, and constructors are public and hence candidates for
 * completion. Now gets all fields and methods, not just those declared
 * by the class.
 *
 */

// End of Completion.java
