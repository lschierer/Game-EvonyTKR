import 'iconify-icon';

import { signalObject } from 'signal-utils/object';
import { reaction } from 'signal-utils/subtle/reaction';

import {
  type Table,
  type ColumnDef,
  type TableOptions,
  type TableOptionsResolved,
  getCoreRowModel,
  getSortedRowModel,
  createTable,
  type TableState,
  type CellContext,
  type HeaderContext,
} from '@tanstack/table-core';

/**
 * TypeScript for enhanced General Pairs table sorting functionality
 */

export type SortDirection = 'asc' | 'desc';

import * as z from 'zod';

export const Attack = z.object({
  base: z.number(),
  increment: z.number(),
});
export type Attack = z.infer<typeof Attack>;

export const BasicAttributes = z.object({
  attack: Attack,
  defense: Attack,
  leadership: Attack,
  politics: Attack,
});
export type BasicAttributes = z.infer<typeof BasicAttributes>;

export const General = z.object({
  ascending: z.boolean(),
  basicAttributes: BasicAttributes,
  builtInBookName: z.string(),
  id: z.string(),
  name: z.string(),
  specialityNames: z.array(z.string()),
  type: z.array(z.string()),
});
export type Ary = z.infer<typeof General>;

export const GeneralPair = z.object({
  attackbuff: z.number(),
  defensebuff: z.number(),
  groundattackdebuff: z.number(),
  grounddefensedebuff: z.number(),
  groundhpdebuff: z.number(),
  hpbuff: z.number(),
  marchbuff: z.number(),
  mountedattackdebuff: z.number(),
  mounteddefensedebuff: z.number(),
  mountedhpdebuff: z.number(),
  primary: General,
  rangedattackdebuff: z.number(),
  rangeddefensedebuff: z.number(),
  rangedhpdebuff: z.number(),
  secondary: General,
  siegeattackdebuff: z.number(),
  siegedefensedebuff: z.number(),
  siegehpdebuff: z.number(),
});
export type GeneralPair = z.infer<typeof GeneralPair>;

export const flexRender = <TProps extends object>(comp: any, props: TProps) => {
  if (typeof comp === 'function') {
    return comp(props);
  }
  return comp;
};

/**
 * Signal-aware table wrapper around TanStack Table.
 */
export const useTable = <GeneralPair>(options: TableOptions<GeneralPair>) => {
  // Create an internal reactive state object based on initial table state
  let state = signalObject({
    columnPinning: {
      left: new Array<string>(),
      right: new Array<string>(),
    },
    ...options.state,
  }) as Partial<TableState>;

  // Compose user options into TanStack's resolved options
  const resolvedOptions: TableOptionsResolved<GeneralPair> = {
    state: state,
    onStateChange: () => {}, // placeholder
    renderFallbackValue: null,
    ...options,
  };

  const table = createTable<GeneralPair>(resolvedOptions);

  // Watch the internal state and re-bind it to the table options on change
  reaction(
    () => state as unknown as TableState, // tslint:disable-line
    (value: TableState) => {
      table.setOptions((prev) => ({
        ...prev,
        ...options,
        state: {
          ...value,
          ...options.state, // user-supplied state wins
        },
        onStateChange: (updater) => {
          const newState =
            typeof updater === 'function'
              ? updater(state as unknown as TableState)
              : updater; // tslint:disable-line
          Object.assign(state, newState); // update SignalObject in-place
          options.onStateChange?.(updater);
        },
      }));
    },
  );

  return table;
};

const tableHeaders = new Map<string, string>([
  ['primary', 'Primary'],
  ['secondary', 'Secondary'],
  ['marchbuff', 'March Size'],
  ['attackbuff', 'Attack'],
  ['defensebuff', 'Defense'],
  ['hpbuff', 'HP'],
  ['groundattackdebuff', 'Ground Attack Debuff'],
  ['grounddefensedebuff', 'Ground Defense Debuff'],
  ['groundhpdebuff', 'Ground HP Debuff'],
  ['mountedattackdebuff', 'Mounted Attack Debuff'],
  ['mounteddefensedebuff', 'Mounted Defense Debuff'],
  ['mountedhpdebuff', 'Mounted HP Debuff'],
  ['rangedattackdebuff', 'Ranged Attack Debuff'],
  ['rangeddefensedebuff', 'Ranged Defense Debuff'],
  ['rangedhpdebuff', 'Ranged HP Debuff'],
  ['siegeattackdebuff', 'Siege Attack Debuff'],
  ['siegedefensedebuff', 'Siege Defense Debuff'],
  ['siegehpdebuff', 'Siege HP Debuff'],
]);

const columns: ColumnDef<GeneralPair>[] = Array.from(tableHeaders.keys()).map(
  (key) => {
    const sortDescFirst = key !== 'primary' && key !== 'secondary';
    const accessorKey =
      key === 'primary' || key === 'secondary' ? `${key}.name` : key;

    return {
      id: key,
      accessorKey,
      cell: (info: CellContext<GeneralPair, unknown>) => {
        const value = info.getValue();
        return typeof value === 'number'
          ? value.toLocaleString()
          : (value ?? '');
      },
      header: (info: HeaderContext<GeneralPair, unknown>) =>
        `<span>${tableHeaders.get(info.column.id)}</span>`,
      sortDescFirst,
    };
  },
);

const renderTable = (table: Table<GeneralPair>) => {
  // Create table elements
  const tableElement = document.createElement('table');
  const theadElement = document.createElement('thead');
  const tbodyElement = document.createElement('tbody');

  tableElement.classList.add('spectrum-Table');
  tableElement.classList.add('spectrum-Table--sizeM');
  theadElement.classList.add('spectrum-Table-head');
  tbodyElement.classList.add('spectrum-Table-body');

  tableElement.appendChild(theadElement);
  tableElement.appendChild(tbodyElement);

  if (!table) {
    console.error('Table is undefined!!');
    return;
  }

  // Render table headers
  table.getHeaderGroups().forEach((headerGroup) => {
    const trElement = document.createElement('tr');

    headerGroup.headers.forEach((header) => {
      const thElement = document.createElement('th');
      thElement.classList.add('spectrum-Table-headCell', 'is-sortable');
      thElement.colSpan = header.colSpan;

      const divElement = document.createElement('div');
      divElement.classList.add('w-500', 'cursor-pointer', 'select-none');

      if (!header.isPlaceholder) {
        // Set inner HTML and click handler
        divElement.innerHTML = flexRender(
          header.column.columnDef.header,
          header.getContext(),
        );

        // Append sort indicator
        const sortIcon =
          {
            asc: ' 🔼',
            desc: ' 🔽',
          }[header.column.getIsSorted() as string] ?? '';
        divElement.innerHTML += sortIcon;

        // Wire up sorting toggle
        divElement.addEventListener('click', (event) => {
          const isMultiSort = event.shiftKey;
          header.column.toggleSorting(isMultiSort);
        });
      }

      thElement.appendChild(divElement);
      trElement.appendChild(thElement);
    });

    theadElement.appendChild(trElement);
  });

  // Render table rows
  table
    .getRowModel()
    .rows.slice(0, 10)
    .forEach((row) => {
      const trElement = document.createElement('tr');
      trElement.classList.add('spectrum-Table-row');
      row.getVisibleCells().forEach((cell) => {
        const tdElement = document.createElement('td');
        tdElement.classList.add('spectrum-Table-cell');
        tdElement.innerHTML = flexRender(
          cell.column.columnDef.cell,
          cell.getContext(),
        );
        trElement.appendChild(tdElement);
      });
      tbodyElement.appendChild(trElement);
    });

  // Render table state info
  const stateInfoElement = document.createElement('pre');
  stateInfoElement.textContent = JSON.stringify(
    {
      sorting: table.getState().sorting,
    },
    null,
    2,
  );

  // Clear previous content and append new content
  const wrapperElement = document.getElementById('wrapper') as HTMLDivElement;
  wrapperElement.innerHTML = '';
  wrapperElement.appendChild(tableElement);
  wrapperElement.appendChild(stateInfoElement);
};

async function fetchData(): Promise<GeneralPair[]> {
  const url = window.location.pathname.replace(/\/$/, '') + '/data.json';
  const response = await fetch(url);
  if (!response.ok) throw new Error('Failed to fetch data');

  const json = await response.json();
  const valid = GeneralPair.array().safeParse(json.data as object);
  if (valid.success) {
    return valid.data;
  }
  console.error('error getting data: ', valid.error.message);
  return [];
}

fetchData().then((data) => {
  const table: Table<GeneralPair> = useTable<GeneralPair>({
    data,
    columns,
    getCoreRowModel: getCoreRowModel(),
    getSortedRowModel: getSortedRowModel(),
    enableSorting: true,
    enableMultiSort: true,
    onStateChange: () => renderTable(table),
  });

  renderTable(table);
});
