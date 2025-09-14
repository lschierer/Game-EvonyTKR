const DEBUG: boolean = false;
// eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
if (DEBUG) {
  console.log(`DEBUG enabled for ${new URL(import.meta.url).pathname}`);
}

const fileDebug: Record<string, boolean> = {
  'lib/Generals/GeneralTable.ts': false,
  'lib/Generals/Pair/data.ts': true,
  'lib/Generals/Pair/filter.ts': true,
  'lib/Generals/Pair/pairStore.ts': true,
  'lib/Generals/PairTable.ts': false,
  'lib/Generals/PairTableContext.ts': false,
  'lib/Generals/PairTableData.ts': false,
  'lib/Generals/PairTableState.ts': false,
  'lib/Generals/pair_level_settings_form.ts': false,
  'lib/Generals/specialtyStore.ts': true,
  'lib/partials/level_settings_form.ts': false,
  'lib/Generals/Pair/PairPicker.ts': true,
};

function isAbsolutePath(path: string): boolean {
  // Check for Unix-style absolute paths (starts with `/`)
  if (path.startsWith('/')) return true;

  // Check for Windows-style absolute paths (e.g., `C:\Users\Example`)
  if (/^[a-zA-Z]:[\\/]/.test(path)) return true;

  // Check for absolute URLs
  try {
    new URL(path);
    return true;
  } catch {
    return false;
  }
}

const debugFunction = (myName: string): boolean => {
  if (isAbsolutePath(myName)) {
    let root = '';

    const rootStack = new URL(import.meta.url).pathname.split('/');
    root = rootStack.slice(0, -2).join('/');

    myName = myName.replace(root, '');
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
    if (DEBUG) {
      console.log(`new name is ${myName}, root was ${root}`);
    }
  } else {
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
    if (DEBUG) {
      console.log(`got path ${myName}`);
    }
  }
  return !!fileDebug[myName];
};

export default debugFunction;
