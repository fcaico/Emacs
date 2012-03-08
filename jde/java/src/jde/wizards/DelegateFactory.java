/*
 * Copyright (c) Eric D. Friedman 2000. All Rights Reserved.
 * Copyright (c) Paul Kinnucan 2000. All Rights Reserved.
 * Copyright (c) Charles Hart 2000. All Rights Reserved.
 *
 * $Revision: 1.3 $ 
 * $Date: 2000/08/03 04:31:20 $ 
 *
 * DelegateFactory is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 *
 * DelegateFactory is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * To obtain a copy of the GNU General Public License write to the
 * Free Software Foundation, Inc.,  59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.  
 */

package jde.wizards;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.io.PrintWriter;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;

/**
 * Defines a factory for creating  skeleton implementations of 
 * Java interfaces. The factory can be invoked from the command line
 * or from another program. The factory can generate implementations for
 * multiple interfaces when invoked from the command line.
 *
 * @author Charles Hart, Eric D. Friedman, and Paul Kinnucan
 * @version $Revision: 1.3 $
 */

public class DelegateFactory extends MethodFactory
{

  /** A table w/ declaring classes as keys and vectors of method
   * signatures as values */
  private Hashtable interfaces = new Hashtable();

  /** The interface factory. */
  static DelegateFactory delegateFactory;
  
  public DelegateFactory() {}

  /** 
   * Creates an DelegateFactory that uses the specified NameFactory
   * for generating parameter names 
   *
   * @param factory Factory for generating parameter names
   */
  public DelegateFactory(NameFactory factory)
  {
    super(factory);
  }

  /** 
   * Adds a signature to the signature table.
   * The signatures are stored in Vectors with their declaring
   * Classes as keys in a hashtable.  This allows them to be pulled
   * out in groups when we print, and keeping the declaring Class info
   * lets us put in comments about where the method (or method group)
   * comes from.  Signatures are not added if they're already registered
   * because we do not want duplicate method implementations even though
   * a class might implement interfaces that inherit from some common
   * super-interface. 
   *
   * @param sig Signature to be stored in the signature table.
   */
  private final void sortByDeclaringClass(Signature sig)
  {
    String declaring = sig.getDeclaringClass().getName();
    if (interfaces.containsKey( declaring ))
    {
      Vector v = (Vector)interfaces.get( declaring );
      if (! v.contains( sig ) ) // "There can be only one" - the Highlander
        v.addElement(sig);
    } // end of if (interfaces.containsKey( dec ))
    else
    {
      Vector v = new Vector();
      v.addElement(sig);
      interfaces.put( declaring, v );
    } // end of else
  }


  /** 
   * Clears the import and interface hashtables for this factory so they
   * can be re-used to process a new set of interfaces.
   */
  public void flush()
  {
    super.flush();
    interfaces.clear();
  }

  
  /**
   * Generates signatures based on introspection of the specified interface. 
   * Strips package specifiers from generated signatures.
   *
   * @param name the interface to process for signatures.
   * @exception java.lang.ClassNotFoundException the requested class cannot be loaded 
   */
  public void process(String interfaceName)
    throws ClassNotFoundException
  {
    process(interfaceName, true);
  }  
  
  /**
   * Generates signatures based on introspection of the specified class. 
   *
   * @param name the interface to process for signatures.
   * @param truncate toggles truncation of package specifiers in signatures..
   *
   * @exception java.lang.ClassNotFoundException the requested class cannot
   * be loaded 
  */
  public void process(String name, boolean truncate)
    throws ClassNotFoundException
  {
    if (null == namefactory)
      namefactory = new DefaultNameFactory();
    
    Class aclass = Class.forName( name );
    
    Method[] methods = aclass.getMethods();
    for (int i = 0; i < methods.length; i++)
      sortByDeclaringClass( new Signature( methods[i], this, truncate, true ));
  }


  /**
   * Makes an implementation of an interface. This method delegates the creation
   * of the implementation to makeInterfaceInternal.
   *
   * @param name Name of interface to be implemented.
   * @param javadoc If <code>true</code> generate skeletal Javadoc for the implementation.
   * @param truncate If <code>true</code>, truncate package specifier when generating code.
   * @param newline If <code>true</code>, insert a newline after opening brace.
   * 
   */
  public static void makeDelegates(String attributeName,
                                   String className, 
                                   boolean javadoc,
                                   boolean truncate,
                                   boolean newline) {

    if (delegateFactory == null)
      delegateFactory = new DelegateFactory();

    delegateFactory.flush();
    delegateFactory.makeInterfaceInternal(attributeName, className, javadoc, 
                                          truncate, newline);

  }

 /**
   * Makes an implementation of an interface.
   *
   * @param name Name of interface to be implemented.
   * @param javadoc If <code>true</code> generate skeletal Javadoc for the implementation.
   * @param truncate If <code>true</code>, truncate package specifier when generating code.
   * @param newline If <code>true</code>, insert a newline after opening brace.
   * 
   */
  private void makeInterfaceInternal(String attributeName,
                                     String className,
                                     boolean javadoc,
                                     boolean truncate,
                                     boolean newline) {
    try {
      process(className, truncate);
    }
    catch (ClassNotFoundException e) {
      println("(error \"Error: could not find class named: " + className + ". "
	      + "Note: name must be qualified.\")");
      return;
    }
    catch (Exception e) {
      println("(error \"Error: unknown type.\")");
      return;
    }

    dump(new PrintWriter( System.out, true),
         attributeName,
         javadoc,
         truncate,
         newline );
  }


  public static void getImportedClasses() {
    String res = "(list ";
    Enumeration i = delegateFactory.imports.keys();
    while (i.hasMoreElements()) {
      Class c = (Class) i.nextElement();
      res += "\"" + c.getName() + "\" ";
    }
    res += ")";
    println(res);
  }


  public String getMethodSkeleton(Signature sig,
                                  boolean javadoc,
                                  boolean newline,
                                  String attributeName)
  {
    String res = "";
    res += "\n" ;
    if (javadoc)
      res += sig.toJavaDoc() + "\n";
    if (newline)
      {
	res += sig  + "\n";
	res += "{\n";
      }
    else
      res += sig + " {\n";
          
    Method m = sig.getMethod();
    Class cl = m.getReturnType();

    if (! cl.getName().equals("void"))
      res += "  return "+attributeName+"."+m.getName()+
             "("+sig.getParameterNames(m.getParameterTypes())+");\n";
    else
      res += "  "+attributeName+"."+m.getName()+
             "("+sig.getParameterNames(m.getParameterTypes())+");\n";

    res += "}\n";
    return res;
  }

  public void dump(PrintWriter out,
                   String attributeName,
                   boolean javadoc,
                   boolean truncate,
                   boolean newline)
  {
    StringBuffer buf = new StringBuffer("\"");

    Enumeration declaring_classes = interfaces.keys();
    while (declaring_classes.hasMoreElements())
    {
      String interf = (String)declaring_classes.nextElement();
      Vector v = (Vector)interfaces.get(interf);
      buf.append("// Code for delegation of ");
      buf.append(interf);
      buf.append(" methods to ");
      buf.append(attributeName);
      buf.append("\n");
      Enumeration e = v.elements();
      while (e.hasMoreElements())
      {
        Signature sig = (Signature)e.nextElement();
        buf.append(getMethodSkeleton(sig, javadoc, newline, attributeName));
      } // end of while (e.hasMoreElements())
    }
    buf.append("\"");
    println(buf.toString());
  }
} // DelegateFactory

/*
 * $Log: DelegateFactory.java,v $
 * Revision 1.3  2000/08/03 04:31:20  paulk
 * Add support for generating a see secton in the Javadoc comment for a method. Thanks to raffael.herzog@comartis.com
 *
 * Revision 1.2  2000/08/01 08:19:25  paulk
 * Fixes bug in dump method .Thanks to eric@hfriedman.rdsl.lmi.net.
 *
 * Revision 1.1  2000/07/14 05:26:55  paulk
 * Adds support for delegation wizard.
 *
 */

// End of DelegateFactory.java
