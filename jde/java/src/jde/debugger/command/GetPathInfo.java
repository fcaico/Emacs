/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.1 $
 */

package jde.debugger.command;
import jde.debugger.JDEException;
import com.sun.jdi.PathSearchingVirtualMachine;
import java.util.Iterator;
import jde.debugger.LispForm;




/**
 * 'get_path_information' command. Returns all the vm knows about
 * paths.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * get_path_information
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmd_id "base-directory" (list [boot-class-path component]*) (list [class-path component]*))
 * </pre>
 *
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.1 $
 *
 */
public class GetPathInfo extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {

    if (!(proc.getVM() instanceof PathSearchingVirtualMachine))
      throw new JDEException("VM doesn't search paths");

    PathSearchingVirtualMachine vm =
	    (PathSearchingVirtualMachine)proc.getVM();

    String bootClassPathString = "(list";
    Iterator it = vm.bootClassPath().iterator();
    while (it.hasNext()) {
      bootClassPathString += " \""+it.next()+"\"";
    }
    bootClassPathString += ")";
    
    bootClassPathString = bootClassPathString.replace('\\', '/');
    
    String classPathString = "(list";
    it = vm.classPath().iterator();
    while (it.hasNext()) {
      classPathString += " \""+it.next()+"\"";
    }
    classPathString += ")";
    
    classPathString = classPathString.replace('\\', '/');
    
    jde.signalCommandResult(procID, cmdID,
			     new LispForm("\""+vm.baseDirectory().replace('\\', '/')+"\""
					  + BR +bootClassPathString
					  + BR +classPathString));
  }

  public Object clone() {return new Finish();}
  
} // GetPathInfo

/*
 * $Log: GetPathInfo.java,v $
 * Revision 1.1  2001/03/24 05:52:14  paulk
 * Initial version.
 *
 *
 */

// End of GetPathInfo.java
