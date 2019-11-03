document.addEventListener('DOMContentLoaded', function () {
    const dashboardDOMElement = document.querySelector('.js-dashboard');
    const results = JSON.parse(localStorage.getItem('HolyJS-kontur-name-that-tune'));
    let dashboardContent;

    if (!!results) {
        const resultsString = results
            .map(result => `<tr><td>${result.name}</td><td>${result.email}</td><td>${result.score}</td></tr>`)
            .reduce((sum, current) => sum + current);

        dashboardContent = document.createElement('table');
        dashboardContent.innerHTML = resultsString;
    } else {
        dashboardContent = 'Не найдено результатов'
    }
    dashboardDOMElement.appendChild(dashboardContent);
});
