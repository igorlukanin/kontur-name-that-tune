document.addEventListener('DOMContentLoaded', function () {
    const submitRegistrationForm = document.querySelector('.js-submit-form');
    const emailInput = document.querySelector('.js-email-input');
    const nameInput = document.querySelector('.js-name-input');
    const form = document.querySelector('.js-form');

    let currentUser = {};

    const submitForm = (e, data) => {
        e.preventDefault();
        currentUser = data;
        form.reset();
    };

    submitRegistrationForm.addEventListener('click', function (e) {
        const data = {
            name: nameInput.value,
            email: emailInput.value,
        };
        emailInput.validity.valid && nameInput.validity.valid ? submitForm(e, data) : null;
    })
});
