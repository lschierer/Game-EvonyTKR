import 'iconify-icon';
/**
 * TypeScript for enhanced General Pairs table sorting functionality
 */

interface SortState {
  columns: string[];
  directions: Record<string, 'asc' | 'desc'>;
}

document.addEventListener('DOMContentLoaded', () => {
  // Initialize sort state from the current URL
  const urlParams = new URLSearchParams(window.location.search);
  const sortParam = urlParams.get('sort') || 'primary,secondary';
  const dirParam = urlParams.get('dir') || 'desc,desc';

  const sortColumns = sortParam.split(',');
  const sortDirs = dirParam.split(',');

  const sortState: SortState = {
    columns: sortColumns,
    directions: {},
  };

  // Initialize directions
  sortColumns.forEach((col, index) => {
    sortState.directions[col] = (sortDirs[index] || 'desc') as 'asc' | 'desc';
  });

  // Add click event listeners to sortable headers
  const sortableHeaders = document.querySelectorAll<HTMLElement>(
    '.spectrum-Table-headCell.is-sortable',
  );

  sortableHeaders.forEach((header) => {
    const column = header.dataset.column;
    if (!column) return;

    header.addEventListener('click', (event) => {
      // Handle shift-click for multi-column sorting
      if (event.shiftKey) {
        handleShiftClick(column, sortState);
      } else {
        // Regular click - toggle direction or reset to single column
        handleRegularClick(column, sortState);
      }

      // Update the URL and navigate
      updateUrlAndNavigate(sortState);
    });
  });
});

function handleRegularClick(column: string, sortState: SortState): void {
  // If this column is already the primary sort column, just toggle its direction
  if (sortState.columns[0] === column) {
    sortState.directions[column] =
      sortState.directions[column] === 'asc' ? 'desc' : 'asc';
  } else {
    // Otherwise, make this the only sort column (reset multi-column sort)
    const direction = sortState.directions[column] === 'asc' ? 'desc' : 'asc';
    sortState.columns = [column];
    sortState.directions = { [column]: direction };
  }
}

function handleShiftClick(column: string, sortState: SortState): void {
  // If column is already in the sort order, toggle its direction
  const existingIndex = sortState.columns.indexOf(column);

  if (existingIndex >= 0) {
    sortState.directions[column] =
      sortState.directions[column] === 'asc' ? 'desc' : 'asc';
  } else {
    // Add this column to the end of the sort order
    sortState.columns.push(column);
    sortState.directions[column] = 'desc'; // Default to descending for new columns
  }
}

function updateUrlAndNavigate(sortState: SortState): void {
  const sortParam = sortState.columns.join(',');
  const dirParam = sortState.columns
    .map((col) => sortState.directions[col])
    .join(',');

  const url = new URL(window.location.href);
  url.searchParams.set('sort', sortParam);
  url.searchParams.set('dir', dirParam);

  window.location.href = url.toString();
}

export {};
