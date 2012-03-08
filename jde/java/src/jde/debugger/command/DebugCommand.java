/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.2 $
 */

package jde.debugger.command;
import java.util.List;
import jde.debugger.JDEException;
import jde.debugger.Protocol;
import jde.debugger.Jdebug;
import jde.debugger.Debug;
import jde.debugger.JDE;
import jde.debugger.ProcessRegistry;


/**
 * Class of debugger commands.
 *
 * Command-line syntax:
 *
 * app_id cmd_id cmd_name [arg]*
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 */

abstract public class DebugCommand extends Thread 
  implements Protocol, Cloneable {
  
  public DebugCommand() {super("JDEbug command");}

  public void init(Integer procID, Integer cmdID, 
		   String cmdName, List args) throws JDEException {
    this.procID = procID;
    this.cmdID = cmdID;
    this.cmdName = cmdName;
    this.args = args;

    setName("JDEbug command(" + procID + " " + cmdID + " " + cmdName + ")");

  }

  abstract protected void doCommand() throws JDEException;


  public void run() {

    CommandRegistry commandRegistry = CommandRegistry.getTheRegistry();

    // see if there already is a command with this cmd_id. this
    // should never happen.
    if (commandRegistry.commandExists(cmdID)) {
      jde.signalCommandError(Jdebug.debuggerID, 
			     Jdebug.debuggerID, 
			     "Duplicate cmd_id '" + cmdID + "'");
      return;
    }

    // if not, add to pending commands.
    commandRegistry.addCommand(cmdID);
    

    try {
      doCommand();
    }
    catch (JDEException ex) {
      Debug.printIf(ex);
      // a jde exception was raised. the kind of error is already
      // in there.
      jde.signalCommandError(Jdebug.debuggerID, cmdID, ex.getMessage());
      return;
    } 
    catch (Exception ex) {
      Debug.printIf(ex);
      jde.signalCommandError(Jdebug.debuggerID, cmdID, "Unspecified error: "+ex.toString());
      return;
    } 
    finally {
      commandRegistry.removeCommand(cmdID);
    }

  }

  abstract public Object clone();

  Integer procID;

  Integer cmdID;

  String cmdName;

  List args;

  protected JDE jde = JDE.getJDE();

  protected Jdebug jdebug = Jdebug.getTheDebugger();

  protected ProcessRegistry procRegistry = ProcessRegistry.getRegistry();
  
} // DebugCommand


/*
 * $Log: DebugCommand.java,v $
 * Revision 1.2  2001/03/24 05:42:36  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.1  2000/01/30 12:31:51  paulk
 * Initial revision.
 *
 */

// End of DebugCommand.java
