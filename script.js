document.addEventListener('DOMContentLoaded', function () {
    const submitRegistrationForm = document.querySelector('.js-submit-form');
    const emailInput = document.querySelector('.js-email-input');
    const nameInput = document.querySelector('.js-name-input');
    const form = document.querySelector('.js-form');
    const formFrame = document.querySelector('.js-frame_registration');

    let currentUser = {};

    const shuffle = (array) => {
        let j, x, i;
        for (i = array.length - 1; i > 0; i--) {
            j = Math.floor(Math.random() * (i + 1));
            x = array[i];
            array[i] = array[j];
            array[j] = x;
        }

        return array;
    };

    const submitForm = (e, data) => {
        const questions = [
            new Question('question 0', ['line0', 'line1', 'line2'], 'correctAnswer', ['correctAnswer', 'possible answer']),
            new Question('question 1', ['line0', 'line1', 'line2'], 'correctAnswer', ['correctAnswer', 'possible answer']),
            new Question('question 2', ['line0', 'line1', 'line2'], 'correctAnswer', ['correctAnswer', 'possible answer2']),
        ];

        e.preventDefault();
        currentUser = data;
        form.reset();
        formFrame.classList.toggle('hidden');

        const game = new Game(currentUser,
            {
                gameLength: 4,
                maxRoundScore: 10,
                questions: questions
            });

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
            this.currentRound = 0;

            this.user = user;
            this.rounds = shuffle(config.questions)
                .slice(0, config.gameLength)
                .map(x => new Round(x, config.maxRoundScore));

            this.rulesFrame = document.querySelector('.js-frame_rules');
            this.startGameBtn = document.querySelector('.js-start-game');
            this.gameElement = document.querySelector('.js-game-window');

            this.startGameBtn.addEventListener('click', () => {
                this.rulesFrame.classList.toggle('hidden');
                this.run();
            });
        }

        showRules() {
            this.rulesFrame.classList.toggle('hidden');
        }

        run() {
            for (let i = 0; i < this.rounds.length; ++i) {
                this.renderRound(this.rounds[i]);
            }
        }

        nextRound() {
            if (this.currentRound < this.rounds.length - 1) {
                this.renderRound(this.rounds[this.currentRound]);
                this.currentRound += 1;
            } else {
                this.renderResult();
            }
        }

        renderResult() {
            const result = this.rounds
                .map(x => x.roundScore)
                .reduce((a, x) => a + x);

            const resultText = this.createParagraph('Your score: ' + result);
            this.removeChildren(this.gameElement);
            this.gameElement.appendChild(resultText);
            // todo
        }

        renderRound(round) {    // Round
            const container = document.createElement('div');

            container.appendChild(this.createParagraph(round.question.question));
            this.renderCodeSample(container, round);
            this.renderAnswers(container, round);

            this.removeChildren(this.gameElement);
            this.gameElement.appendChild(container);
        }

        removeChildren(node) {
            while (node.firstChild) {
                node.removeChild(node.firstChild);
            }
        }

        renderCodeSample(
            parentElement,  // element
            round) {        // Round
            const codeContainer = document.createElement('div');
            const sampleLines = round.getSampleLines();

            for (let i = 0; i < sampleLines.length; ++i) {
                codeContainer.appendChild(this.createParagraph(sampleLines[i]));
            }

            const button = this.createButton('Show one more line', () => {
                round.showOneMoreLine();
                this.renderRound(round);
            });

            parentElement.appendChild(button);
            parentElement.appendChild(codeContainer);
        }

        renderAnswers(
            parentElement,  // element
            round) {        // Round
            const buttonsContainer = document.createElement('div');
            const possibleAnswers = round.question.possibleAnswers;

            for (let i = 0; i < possibleAnswers.length; ++i) {
                const possibleAnswer = possibleAnswers[i];
                const button = this.createButton(possibleAnswer, () => {
                    round.acceptAnswer(possibleAnswer);
                    this.nextRound();
                });

                buttonsContainer.appendChild(button);
            }

            parentElement.appendChild(buttonsContainer);
        }

        createButton(text, listener) {
            const button = document.createElement('button');
            button.innerText = text;

            if (listener) {
                button.addEventListener('click', listener);
            }

            return button;
        }

        createParagraph(text) {
            const questionText = document.createElement('p');
            const content = document.createTextNode(text);
            questionText.appendChild(content);

            return questionText;
        }
    }

    class Question {
        constructor(question, sampleLines, correctAnswer, possibleAnswers) {
            this.question = question;               // string
            this.sampleLines = sampleLines;         // string[]
            this.correctAnswer = correctAnswer;     // string
            this.possibleAnswers = possibleAnswers; // string[]
        }
    }

    class Round {
        constructor(question, maxScore) {
            this.maxScore = maxScore;               // int
            this.roundScore = 0;                    // int
            this.visibleLines = 1;                  // int
            this.question = question;               // Question
        }

        getSampleLines() {
            return this.question.sampleLines.slice(0, this.visibleLines);
        }

        showOneMoreLine() {
            if (this.visibleLines < this.question.sampleLines.length) {
                this.visibleLines += 1;
            }
        }

        acceptAnswer(answer) {
            if (answer === this.question.correctAnswer) {
                switch (this.visibleLines) {
                    case 1:
                        this.roundScore = this.maxScore * 1.0;
                        break;

                    case 2:
                        this.roundScore = this.maxScore * 0.75;
                        break;

                    case 3:
                        this.roundScore = this.maxScore * 0.50;
                        break;

                    case 4:
                        this.roundScore = this.maxScore * 0.25;
                        break;

                    default:
                        this.roundScore = 0.0;
                        break;
                }
            }
        }
    }
});
