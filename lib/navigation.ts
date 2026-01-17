// Navigation tree expand/collapse functionality for Spectrum TreeView

document.addEventListener('DOMContentLoaded', () => {
  const navigationTree = document.querySelector('.spectrum-Site-sideBar nav');

  if (navigationTree) {
    navigationTree
      .querySelectorAll('.spectrum-TreeView-item')
      .forEach((item) => {
        const chevron = item.querySelector<HTMLElement>(
          ':scope > .spectrum-TreeView-itemLink > .spectrum-TreeView-itemIndicator'
        );
        const childList = item.querySelector<HTMLElement>(
          ':scope > .spectrum-TreeView'
        );

        if (chevron && childList) {
          // Make chevron clickable
          chevron.style.cursor = 'pointer';
          chevron.addEventListener('click', (e) => {
            e.preventDefault();
            e.stopPropagation();

            // Check if currently collapsed by looking at the child ul
            const isCollapsed = childList.classList.contains('nav-collapsed');

            if (isCollapsed) {
              // Expand: add is-open to li, remove nav-collapsed from ul
              item.classList.add('is-open');
              item.setAttribute('aria-expanded', 'true');
              childList.classList.remove('nav-collapsed');

              // Remove nav-hidden from all child items
              childList
                .querySelectorAll('.spectrum-TreeView-item.nav-hidden')
                .forEach((childItem) => {
                  childItem.classList.remove('nav-hidden');
                });
            } else {
              // Collapse: remove is-open from li, add nav-collapsed to ul
              item.classList.remove('is-open');
              item.setAttribute('aria-expanded', 'false');
              childList.classList.add('nav-collapsed');
            }
          });
        }
      });
  }
});
