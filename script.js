document.addEventListener('DOMContentLoaded', function () {
    const submitRegistrationForm = document.querySelector('.js-submit-form');
    const emailInput = document.querySelector('.js-email-input');
    const nameInput = document.querySelector('.js-name-input');
    const form = document.querySelector('.js-form');
    const formFrame = document.querySelector('.js-frame_registration');

    let currentUser = {};

    const submitForm = (e, data) => {
        e.preventDefault();
        currentUser = data;
        form.reset();
        formFrame.classList.toggle('hidden');

        const game = new Game(currentUser, {});
        game.showRules();
    };

    submitRegistrationForm.addEventListener('click', function (e) {
        const data = {
            name: nameInput.value,
            email: emailInput.value,
        };
        emailInput.validity.valid && nameInput.validity.valid ? submitForm(e, data) : null;
    });

    class Game {
        constructor(user, config) {
            this.user = user;
            this.config = config;
            this.rulesFrame = document.querySelector('.js-frame_rules');
            this.startGameBtn = document.querySelector('.js-start-game');

            this.startGameBtn.addEventListener('click', () => {
                this.rulesFrame.classList.toggle('hidden');
                this.run();
            })
        }

        run() {
            console.log('start game')
        }
        showRules() {
            this.rulesFrame.classList.toggle('hidden');
        }
    }
});
