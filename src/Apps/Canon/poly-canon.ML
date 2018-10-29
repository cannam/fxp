val cwd = OS.FileSys.getDir();

OS.FileSys.chDir "../../";
use "poly-fxpLib.ML";
OS.FileSys.chDir cwd;

use "canonOptions.sml";
use "canonEncode.sml";
use "canonOutput.sml";
use "canonHooks.sml";
use "canon.sml";
