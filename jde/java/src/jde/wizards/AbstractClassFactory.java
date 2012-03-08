/*
 * This class extends the interface factory to handle Abstract classes
 * Copyright (c) Javier Lopez 2001. All Rights Reserved.
 *
 * AbstractClassFactory is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 *
 * AbstractClassFactory is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * To obtain a copy of the GNU General Public License write to the
 * Free Software Foundation, Inc.,  59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.  
 */

package jde.wizards;

import java.io.PrintWriter;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Enumeration;
import java.util.Vector;

/**
 * Defines a factory for creating skeleton implementations of 
 * the abstract methods of Abstract Java classes. The factory
 * can be invoked from the command line or from another program.
 *
 * @author Javier Lopez
 * @version $Revision: 1.1 $
 */
public class AbstractClassFactory extends InterfaceFactory {

  /** The interface factory. */
  static AbstractClassFactory abstractClassFactory;
  
  public AbstractClassFactory() {}

  /** 
   * Creates an AbstractClassFactory that uses the specified NameFactory
   * for generating parameter names 
   *
   * @param factory Factory for generating parameter names
   */
  public AbstractClassFactory(NameFactory factory) {
    super(factory);
  }

  /**
   * Generates signatures based on introspection of the specified interface. 
   * Strips package specifiers from generated signatures.
   *
   * @param argAbstracClassName the abstract class to process for signatures.
   * @exception NotAnAbstractClassException the requested class isn't an abstract class
   * @exception java.lang.ClassNotFoundException the requested class cannot
   * be loaded 
   */
  public void process(String argAbstracClassName)
    throws ClassNotFoundException, NotAnAbstractClassException {
    process(argAbstracClassName, true);
  }  
  
  /**
   * Generates signatures based on introspection of the specified class. 
   *
   * @param name the abstract class to process for signatures.
   * @param truncate toggles truncation of package specifiers in signatures..
   *
   * @exception NotAnAbstractClassException the requested class isn't an abstract class
   * @exception java.lang.ClassNotFoundException the requested class cannot
   * be loaded 
  */
  public void process(String name, boolean truncate)
    throws ClassNotFoundException, NotAnAbstractClassException {
    if (null == namefactory)
      namefactory = new DefaultNameFactory();
    
    Class aclass = Class.forName(name);
    int iModifiers = aclass.getModifiers();
    if (!Modifier.isAbstract(iModifiers))
      throw new NotAnAbstractClassException(name);
    
    Object[] methods = getAbstractMethods(aclass);
    for (int i = 0; i < methods.length; i++)
      sortByDeclaringClass(new Signature((Method)methods[i],
                                         this,
                                         truncate));
  }

  /**
   * Creates a list of the abstract methods in argClass
   *
   * @param argClass Class to obtained the abstract methods from
   * @return a <code>Methods[]</code> containing abstract methods
   */
  private Object[] getAbstractMethods(Class argClass) {
    Method[] methods = argClass.getMethods();
    Method method;
    Vector abstractMethods = new Vector();
    
    for (int i = 0, j = 0; i < methods.length; i++) {
      method = methods[i];
      if (Modifier.isAbstract(method.getModifiers())) {
        abstractMethods.add(method);
      } // end of if (method)
    } // end of for (int i = 0; i < methods.length; i++)
    return abstractMethods.toArray();
  }

  public static void getImportedClasses() {
    String res = "(list ";
    Enumeration i = abstractClassFactory.imports.keys();
    while (i.hasMoreElements()) {
      Class c = (Class) i.nextElement();
      res += "\"" + c.getName() + "\" ";
    }
    res += ")";
    println(res);
  }
  
  /**
   * Makes an implementation of the abstract methods of an abstract class.
   * This method delegates the creation of the implementation to 
   * makeAbstractClassInternal.
   *
   * @param name Name of abstract class to be implemented.
   * @param javadoc If <code>true</code> generate skeletal Javadoc for the implementation.
   * @param truncate If <code>true</code>, truncate package specifier when generating code.
   * @param newline If <code>true</code>, insert a newline after opening brace.
   * 
   */
  public static void makeAbstractClass(String name, 
                                       boolean javadoc,
                                       boolean truncate,
                                       boolean newline) {
    
    if (abstractClassFactory == null)
      abstractClassFactory = new AbstractClassFactory();
    
    abstractClassFactory.flush();
    abstractClassFactory.makeAbstractClassInternal(name, javadoc, truncate, newline);
  }

 /**
   * Makes an implementation of the abstract methods of an abstract class.
   *
   * @param name Name of abstract class to be implemented.
   * @param javadoc If <code>true</code> generate skeletal Javadoc for the implementation.
   * @param truncate If <code>true</code>, truncate package specifier when generating code.
   * @param newline If <code>true</code>, insert a newline after opening brace.
   * 
   */
  private void makeAbstractClassInternal(String name,
                                         boolean javadoc,
                                         boolean truncate,
                                         boolean newline) {
    try {
      process(name, truncate);
    }
    catch (ClassNotFoundException e) {
      println("(error \"Error: could not find abstract class named: " + name + ". "
	      + "Note: name must be qualified.\")");
      return;
    }
    catch (NotAnAbstractClassException e) {
      println("(error \"Error: " + name + " is not an abstract class.\")");
      return;
    }
    catch (Exception e) {
      e.printStackTrace();
      println("(error \"Error: unknown type.\")");
      return;
    }

    dump(new PrintWriter( System.out, true),
	 javadoc,
	 truncate,
	 newline );
  }

}

class NotAnAbstractClassException extends NotAnInterfaceException {
  NotAnAbstractClassException (String name) {
    super(name);
  }
}

/*
 * $Log: AbstractClassFactory.java,v $
 * Revision 1.1  2001/08/04 03:14:06  paulk
 * Initial revision.
 *
 */

// End of AbstractClassFactory
