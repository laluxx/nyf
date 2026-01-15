// Smooth scrolling
document.querySelectorAll('.nav-list a').forEach(link => {
  link.addEventListener('click', (e) => {
    e.preventDefault();
    const target = document.querySelector(link.getAttribute('href'));
    if (target) {
      target.scrollIntoView({ behavior: 'smooth', block: 'start' });
    }
  });
});

// Search functionality
const searchInput = document.getElementById('search');
const navList = document.getElementById('nav-list');
const contentArea = document.getElementById('content-area');

searchInput.addEventListener('input', (e) => {
  const query = e.target.value.toLowerCase();
  const navItems = navList.querySelectorAll('li');
  const functions = contentArea.querySelectorAll('.function');

  navItems.forEach((item, i) => {
    const text = item.textContent.toLowerCase();
    if (text.includes(query)) {
      item.style.display = 'block';
      functions[i].style.display = 'block';
    } else {
      item.style.display = 'none';
      functions[i].style.display = 'none';
    }
  });
});

// Filter functionality
const filterBtns = document.querySelectorAll('.filter-btn');

filterBtns.forEach(btn => {
  btn.addEventListener('click', () => {
    filterBtns.forEach(b => b.classList.remove('active'));
    btn.classList.add('active');

    const filter = btn.dataset.filter;
    const functions = contentArea.querySelectorAll('.function');
    const navItems = navList.querySelectorAll('li');

    functions.forEach((fn, i) => {
      if (filter === 'all') {
        fn.style.display = 'block';
        navItems[i].style.display = 'block';
      } else if (filter === 'todos') {
        const hasTodo = fn.querySelector('.todo-item');
        fn.style.display = hasTodo ? 'block' : 'none';
        navItems[i].style.display = hasTodo ? 'block' : 'none';
      } else if (filter === 'functions') {
        fn.style.display = 'block';
        navItems[i].style.display = 'block';
      }
    });
  });
});
