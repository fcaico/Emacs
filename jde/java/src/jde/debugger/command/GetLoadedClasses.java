/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.1 $
 */

package jde.debugger.command;
import jde.debugger.JDEException;
import com.sun.jdi.ReferenceType;
import jde.debugger.LispForm;
import java.util.Iterator;


/**
 * 'get_loaded_classes' command. Returns a list of all loaded classes
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * get_loaded_classes
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmd_id (list ["type-name"]*))
 * </pre>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.1 $
 *
 */
public class GetLoadedClasses extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    String typeNames = "(list";
    Iterator it = proc.getVM().allClasses().iterator();
    while (it.hasNext()) {
      typeNames += " \""+((ReferenceType)it.next()).name()+"\"";
    }
    typeNames += ")";
    jde.signalCommandResult(procID, cmdID, new LispForm(typeNames));
   }

  public Object clone() {return new GetLoadedClasses();}
  
} // GetLoadedClasses

/*
 * $Log: GetLoadedClasses.java,v $
 * Revision 1.1  2001/03/24 05:52:13  paulk
 * Initial version.
 *
 *
 */

// End of GetLoadedClasses.java
