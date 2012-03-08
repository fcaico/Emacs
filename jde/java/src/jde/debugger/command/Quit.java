/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.2 $
 */

package jde.debugger.command;
import jde.debugger.JDEException;
import jde.debugger.Jdebug;


/**
 * Kills the debugger.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * quit
 * </pre>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 */
public class Quit extends DebugSessionCommand {
  
  protected void doCommand() throws JDEException {
    try {
      procRegistry.shutdownProcesses();
    } catch (Exception ex) {
      // do nothing
    }
    jde.signalCommandResult(Jdebug.debuggerID, cmdID);
    System.exit(0);
   }

  public Object clone() {return new Quit();}
   
} // Quit


/*
 * $Log: Quit.java,v $
 * Revision 1.2  2001/03/24 05:42:37  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.1  2000/01/31 12:47:01  paulk
 * Quit debugger.
 *
 */

// End of Quit.java
