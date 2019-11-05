document.addEventListener('DOMContentLoaded', function () {
    const dashboardDOMElement = document.querySelector('.js-dashboard');
    const results = JSON.parse(localStorage.getItem('HolyJS-kontur-name-that-tune'));
    let dashboardContent;

    if (!!results) {
        const sortedResults = results.sort(function(a, b) {
            return b.score - a.score;
        });
        const resultsString = sortedResults
            .map(result => `<tr><td>${result.name}</td><td>${result.email}</td><td>${result.score}</td></tr>`)
            .reduce((sum, current) => sum + current);

        dashboardContent = document.createElement('table');
        dashboardContent.innerHTML = '<tr><td>Имя</td><td>Почта</td><td>Очки</td></tr>' + resultsString;
    } else {
        dashboardContent = 'Не найдено результатов'
    }
    dashboardDOMElement.appendChild(dashboardContent);
});
