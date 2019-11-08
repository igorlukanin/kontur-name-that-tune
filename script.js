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
        e.preventDefault();
        currentUser = data;
        form.reset();
        formFrame.classList.toggle('hidden');

        const game = new Game(
            currentUser,
            {
                gameLength: 4,
                maxRoundScore: 10,
                questions: questions,
                timer: new Timer({gameTime: 50}),
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

    class Timer {
        timerDOMNode;
        constructor(config) {
            this.timerDOMNode = document.querySelector('.js-timer');
            this.gameTime = config.gameTime;
            this.timerDOMNode.innerHTML = this.gameTime;
        }

        startTimer(gameTime, finishGame){
            this.timer = setInterval(() => {
                this.gameTime = gameTime--;
                if (gameTime <= 0) {
                    this.destroy();
                    this.roundContainerElement = document.querySelector('.js-round-container');
                    this.roundContainerElement.classList.toggle('hidden');

                    finishGame();
                }
                this.timerDOMNode.innerHTML = gameTime;
            },1000);
        }

        run(finishGame) {
            this.startTimer(this.gameTime, finishGame);
        }

        addTime(seconds, finishGame) {
            this.destroy();
            this.startTimer(this.gameTime + seconds, finishGame);
            this.showAddedTime(seconds);
        }

        destroy() {
            clearTimeout(this.timer);
        }

        showAddedTime(seconds) {
            const addedTimeContainer = document.createElement('div');
            addedTimeContainer.innerText = `+${seconds}`;
            addedTimeContainer.className = 'added-time';
            this.timerDOMNode.appendChild(addedTimeContainer);
        }
    }

    class Game {
        constructor(user, config) {
            this.rulesFrame = document.querySelector('.js-frame_rules');
            this.gameFrame = document.querySelector('.js-frame_game');
            this.startGameBtn = document.querySelector('.js-start-game');
            this.gameElement = document.querySelector('.js-game-window');
            this.gameScoreElement = document.querySelector('.js-score');
            this.gameResetButton = document.querySelector('.js-reset');
            this.timerDOMNode = document.querySelector('.js-timer');
            this.timer = config.timer;

            this.nextRoundIndex = 0;

            this.user = user;
            this.rounds = shuffle(config.questions)
                .slice(0, config.gameLength)
                .map(x => new Round(x, config.maxRoundScore, this.timer, this.finish.bind(this)));

            this.startGameBtn.addEventListener('click', () => {
                this.rulesFrame.classList.toggle('hidden');
                this.gameFrame.classList.toggle('hidden');
                this.run();
                this.timer.run(this.finish.bind(this));
            });

            this.gameResetButton.addEventListener('click', () => {
                location.reload();
            });
        }

        finish() {
            this.timer.destroy();
            this.gameResetButton.classList.toggle('hidden');
            this.gameScoreElement.classList.toggle('hidden');
            this.timerDOMNode.classList.toggle('hidden');

            const totalScoreElem = document.createElement('div');
            const totalScore = this.calcTotalScore();
            totalScoreElem.className = 'total-score';
            totalScoreElem.innerText = `Ваш итоговый результат: ${totalScore}`;
            this.gameElement.appendChild(totalScoreElem);

            this.saveData(this.user, totalScore);
        }

        saveData(user, score) {
            const {name, email} = user;
            const results = JSON.parse(localStorage.getItem('HolyJS-kontur-name-that-tune'));
            const userInfo = {name: name, email: email, score: score};
            if (!!results) {
                localStorage.setItem('HolyJS-kontur-name-that-tune', JSON.stringify([userInfo, ...results]));
            } else {
                localStorage.setItem('HolyJS-kontur-name-that-tune', JSON.stringify([userInfo]));
            }
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
            if (this.nextRoundIndex < this.rounds.length - 1) {
                this.renderRound(this.rounds[this.nextRoundIndex]);
                this.nextRoundIndex += 1;
            } else {
                this.renderResult();
            }
        }

        calcTotalScore() {
            return this.rounds
                .map(x => x.roundScore)
                .reduce((a, x) => a + x);
        }

        renderResult() {
            this.clearForm();
            this.renderScore(this.gameScoreElement);
            this.finish();
        }

        renderRound(round) {// Round
            this.clearForm();

            this.renderScore(this.gameScoreElement);

            const container = document.createElement('div');
            container.classList = 'js-round-container';
            container.appendChild(this.createParagraph(round.question.question));
            this.renderCodeSample(container, round);
            this.renderAnswers(container, round);

            this.gameElement.appendChild(container);
        }

        removeChildren(node) {
            while (node.firstChild) {
                node.removeChild(node.firstChild);
            }
        }

        clearForm() {
            this.removeChildren(this.gameScoreElement);
            this.removeChildren(this.gameElement);
        }

        renderScore(parentElement) {
            const result = this.calcTotalScore();
            const score = `Счет: ${result}`;
            parentElement.innerText = score;
        }

        renderCodeSample(
            parentElement, // element
            round  // Round
        ) {
            const codeContainer = document.createElement('pre');
            codeContainer.className = 'code';
            const sampleLines = round.getSampleLines();

            for (let i = 0; i < sampleLines.length; ++i) {
                codeContainer.appendChild(this.createParagraph(sampleLines[i]));
            }

            const button = this.createButton('Показать еще строку', 'btn btn_link', () => {
                round.showOneMoreLine();
                this.renderRound(round);
            });

            parentElement.appendChild(button);
            parentElement.appendChild(codeContainer);
        }

        renderAnswers(
            parentElement,  // element
            round  // Round
        ) {
            const buttonsContainer = document.createElement('div');
            buttonsContainer.className = 'answers-row';
            const possibleAnswers = round.question.possibleAnswers;

            for (let i = 0; i < possibleAnswers.length; ++i) {
                const possibleAnswer = possibleAnswers[i];
                const button = this.createButton(possibleAnswer, 'btn', () => {
                    round.acceptAnswer(possibleAnswer);
                    this.nextRound();
                });

                buttonsContainer.appendChild(button);
            }

            parentElement.appendChild(buttonsContainer);
        }

        createButton(text, className, listener) {
            const button = document.createElement('button');
            button.classList = className;
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
        constructor(question, maxScore, timer, finishGame) {
            this.maxScore = maxScore;               // int
            this.roundScore = 0;                    // int
            this.visibleLines = 1;                  // int
            this.question = question;               // Question
            this.timer = timer;                 // func
            this.finishGame = finishGame;
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
                this.timer.addTime(10, this.finishGame);
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

    const getRandomInt = (max) => {
        return Math.floor(Math.random() * Math.floor(max));
    };

    const prepareQuestion = (language, languages, codeLines) => {
        const AMOUNT_OF_ANSWER_OPTIONS = 4;
        const possibleAnswers = [language];

        while (possibleAnswers.length < AMOUNT_OF_ANSWER_OPTIONS) {
            const randomIndex = getRandomInt(languages.length);
            if (possibleAnswers.indexOf(languages[randomIndex]) === -1) {
                possibleAnswers.push(languages[randomIndex]);
            }
        }

        return new Question(
            'Какой язык программирования представлен?',
            codeLines.split(/\r?\n/),
            language,
            shuffle(possibleAnswers));
    };

    const languages = ['Java', 'Erlang', 'Python', 'Swift', 'Rust', 'Elm', 'Elixir'];

    const questions = [
        prepareQuestion('Java', languages,
            "public class Cat extends Animal {\n" +
            "\n" +
            "  public void makeSound() {\n" +
            "    System.out.println(\"Meow\");\n" +
            "  }\n" +
            "  \n" +
            "  public void makeSound(boolean happy) {\n" +
            "    System.out.println(\"Purrrrrr\");\n" +
            "  }\n" +
            "}"),

        prepareQuestion('Java', languages,
            "public class MyClass {\n" +
            "  public static void main(String[] args) {\n" +
            "    HashMap people = new HashMap();\n" +
            "\n" +
            "    people.put(\"John\", 32);\n" +
            "    people.put(\"Steve\", 30);\n" +
            "    people.put(\"Angie\", 33);\n" +
            "\n" +
            "    for (String i : people.keySet()) {\n" +
            "      System.out.println(\"Name: \" + i + \" Age: \" + people.get(i));\n" +
            "    }\n" +
            "  }\n" +
            "}"),

        prepareQuestion('Java', languages,
            "public class BinaryConverter {\n" +
            "    public static void main(String[] args){\n" +
            "        for(int i = -5; i < 33; i++){\n" +
            "            System.out.println(i + \": \" + toBinary(i));\n" +
            "            System.out.println(i);\n" +
            "            //always another way\n" +
            "            System.out.println(i + \": \" + Integer.toBinaryString(i));\n" +
            "        }\n" +
            "    }\n" +
            "    \n" +
            "    public static String toBinary(int base10Num){\n" +
            "        boolean isNeg = base10Num < 0;\n" +
            "        base10Num = Math.abs(base10Num);        \n" +
            "        String result = \"\";\n" +
            "        \n" +
            "        while(base10Num > 1){\n" +
            "            result = (base10Num % 2) + result;\n" +
            "            base10Num /= 2;\n" +
            "        }\n" +
            "        assert base10Num == 0 || base10Num == 1 : \"value is not <= 1: \" + base10Num;\n" +
            "        \n" +
            "        result = base10Num + result;\n" +
            "        assert all0sAnd1s(result);\n" +
            "        \n" +
            "        if( isNeg )\n" +
            "            result = \"-\" + result;\n" +
            "        return result;\n" +
            "    }\n" +
            "    \n" +
            "    public static boolean all0sAnd1s(String val){\n" +
            "        assert val != null : \"Failed precondition all0sAnd1s. parameter cannot be null\";\n" +
            "        boolean all = true;\n" +
            "        int i = 0;\n" +
            "        char c;\n" +
            "        \n" +
            "        while(all && i < val.length()){\n" +
            "            c = val.charAt(i);\n" +
            "            all = c == '0' || c == '1';\n" +
            "            i++;\n" +
            "        }\n" +
            "        return all;\n" +
            "    }\n" +
            "}"),

        prepareQuestion('Python', languages,
            "def count(S, m, n): \n" +
            "    table = [[0 for x in range(m)] for x in range(n+1)] \n" +
            "  \n" +
            "    for i in range(m): \n" +
            "        table[0][i] = 1\n" +
            "  \n" +
            "    for i in range(1, n+1): \n" +
            "        for j in range(m): \n" +
            "            x = table[i - S[j]][j] if i-S[j] >= 0 else 0\n" +
            "            y = table[i][j-1] if j >= 1 else 0\n" +
            "            table[i][j] = x + y \n" +
            "  \n" +
            "    return table[n][m-1] \n" +
            "  \n" +
            "arr = [1, 2, 3] \n" +
            "m = len(arr) \n" +
            "n = 4\n" +
            "print(count(arr, m, n)) "),

        prepareQuestion('Python', languages,
            "def factorial(n): \n" +
            "    return 1 if (n==1 or n==0) else n * factorial(n - 1);  \n" +
            "  \n" +
            "num = 5; \n" +
            "print(\"Factorial of\",num,\"is\", \n" +
            "factorial(num)) "),

        prepareQuestion('Python', languages,
            "def power(x, y): \n" +
            "    if y==0: \n" +
            "        return 1\n" +
            "    if y%2==0: \n" +
            "        return power(x, y/2)*power(x, y/2) \n" +
            "    return x*power(x, y/2)*power(x, y/2) \n" +
            "  \n" +
            "def order(x): \n" +
            "  \n" +
            "    n = 0\n" +
            "    while (x!=0): \n" +
            "        n = n+1\n" +
            "        x = x/10\n" +
            "    return n \n" +
            "  \n" +
            "def isArmstrong (x): \n" +
            "    n = order(x) \n" +
            "    temp = x \n" +
            "    sum1 = 0\n" +
            "    while (temp!=0): \n" +
            "        r = temp%10\n" +
            "        sum1 = sum1 + power(r, n) \n" +
            "        temp = temp/10\n" +
            "  \n" +
            "    return (sum1 == x) \n" +
            "  \n" +
            "  \n" +
            "x = 153\n" +
            "print(isArmstrong(x)) \n" +
            "x = 1253\n" +
            "print(isArmstrong(x)) "),

        prepareQuestion('Python', languages,
            "def countSort(arr): \n" +
            "    output = [0 for i in range(256)] \n" +
            "  \n" +
            "    count = [0 for i in range(256)] \n" +
            "  \n" +
            "    ans = [\"\" for _ in arr] \n" +
            "  \n" +
            "    for i in arr: \n" +
            "        count[ord(i)] += 1\n" +
            "  \n" +
            "    for i in range(256): \n" +
            "        count[i] += count[i-1] \n" +
            "  \n" +
            "    for i in range(len(arr)): \n" +
            "        output[count[ord(arr[i])]-1] = arr[i] \n" +
            "        count[ord(arr[i])] -= 1\n" +
            "  \n" +
            "    for i in range(len(arr)): \n" +
            "        ans[i] = output[i] \n" +
            "    return ans  \n" +
            "  \n" +
            "arr = \"geeksforgeeks\"\n" +
            "ans = countSort(arr) \n" +
            "print \"Sorted character array is %s\"  %(\"\".join(ans)) "),

        prepareQuestion('Swift', languages,
            "let fileName = \"myFileName.txt\"\n" +
            "        var filePath = \"\"\n" +
            "        \n" +
            "        let dirs:[String] = NSSearchPathForDirectoriesInDomains(FileManager.SearchPathDirectory.documentDirectory, FileManager.SearchPathDomainMask.allDomainsMask, true)\n" +
            "        \n" +
            "        if dirs.count > 0 {\n" +
            "            let dir = dirs[0] //documents directory\n" +
            "            filePath = dir.appending(\"/\" + fileName)\n" +
            "            print(\"Local path = \\(filePath)\")\n" +
            "        } else {\n" +
            "            print(\"Could not find local directory to store file\")\n" +
            "            return\n" +
            "        }\n" +
            "        \n" +
            "        let fileContentToWrite = \"Text to be recorded into file\"\n" +
            "        \n" +
            "        do {\n" +
            "            try fileContentToWrite.write(toFile: filePath, atomically: false, encoding: String.Encoding.utf8)\n" +
            "        }\n" +
            "        catch let error as NSError {\n" +
            "            print(\"An error took place: \\(error)\")\n" +
            "        }"),

        prepareQuestion('Swift', languages,
            "class ViewController: UIViewController  {\n" +
            "override func viewDidLoad() {\n" +
            "    super.viewDidLoad()\n" +
            "}\n" +
            "override func viewWillAppear(_ animated: Bool) {\n" +
            "    super.viewWillAppear(animated)\n" +
            "    \n" +
            "    var resourceFileDictionary: NSDictionary?\n" +
            "    \n" +
            "    if let path = Bundle.main.path(forResource: \"Info\", ofType: \"plist\") {\n" +
            "        resourceFileDictionary = NSDictionary(contentsOfFile: path)\n" +
            "    }\n" +
            "    \n" +
            "    if let resourceFileDictionaryContent = resourceFileDictionary {\n" +
            "        print(\"MinimumOSVersion = \\(resourceFileDictionaryContent.object(forKey: \"MinimumOSVersion\")!)\")\n" +
            "        print(resourceFileDictionaryContent)\n" +
            "    }\n" +
            "  }\n" +
            "}"),

        prepareQuestion('Rust', languages,
            "struct Sheep { naked: bool, name: &'static str }\n" +
            "\n" +
            "trait Animal {\n" +
            "    fn new(name: &'static str) -> Self;\n" +
            "\n" +
            "    fn name(&self) -> &'static str;\n" +
            "    fn noise(&self) -> &'static str;\n" +
            "\n" +
            "    fn talk(&self) {\n" +
            "        println!(\"{} says {}\", self.name(), self.noise());\n" +
            "    }\n" +
            "}\n" +
            "\n" +
            "impl Sheep {\n" +
            "    fn is_naked(&self) -> bool {\n" +
            "        self.naked\n" +
            "    }\n" +
            "\n" +
            "    fn shear(&mut self) {\n" +
            "        if self.is_naked() {\n" +
            "            println!(\"{} is already naked...\", self.name());\n" +
            "        } else {\n" +
            "            println!(\"{} gets a haircut!\", self.name);\n" +
            "\n" +
            "            self.naked = true;\n" +
            "        }\n" +
            "    }\n" +
            "}"),

        prepareQuestion('Rust', languages,
            "struct A;\n" +
            "\n" +
            "struct Single(A);\n" +
            "struct SingleGen<T>(T);\n" +
            "\n" +
            "fn main() {\n" +
            "    let _s = Single(A);\n" +
            "    \n" +
            "    let _char: SingleGen<char> = SingleGen('a');\n" +
            "\n" +
            "    let _t    = SingleGen(A); // Uses `A` defined at the top.\n" +
            "    let _i32  = SingleGen(6); // Uses `i32`.\n" +
            "    let _char = SingleGen('a'); // Uses `char`.\n" +
            "}"),

        prepareQuestion('Rust', languages,
            "fn main() {\n" +
            "    let n = 5;\n" +
            "\n" +
            "    if n < 0 {\n" +
            "        print!(\"{} is negative\", n);\n" +
            "    } else if n > 0 {\n" +
            "        print!(\"{} is positive\", n);\n" +
            "    } else {\n" +
            "        print!(\"{} is zero\", n);\n" +
            "    }\n" +
            "\n" +
            "    let big_n =\n" +
            "        if n < 10 && n > -10 {\n" +
            "            println!(\", and is a small number, increase ten-fold\");\n" +
            "            10 * n\n" +
            "        } else {\n" +
            "            println!(\", and is a big number, halve the number\");\n" +
            "\n" +
            "            n / 2\n" +
            "        };\n" +
            "    println!(\"{} -> {}\", n, big_n);"),

        prepareQuestion('Erlang', languages,
            "-module(ordmaps).\n" +
            "\n" +
            "-export(\n" +
            "   [new/1,\n" +
            "    add/3,\n" +
            "   ]).\n" +
            "-callback new() -> data().\n" +
            "-callback add(key(), value(), data()) -> data() | no_return().\n" +
            "\n" +
            "-spec new(map_type()) -> ordmap().\n" +
            "new(map) ->\n" +
            "    ImplMod = ordmaps_impl_maps,\n" +
            "    Data = ImplMod:new(),\n" +
            "    #?MODULE{impl_mod=ImplMod, data=Data};\n" +
            "new(gb_tree) ->\n" +
            "    ImplMod = ordmaps_impl_gb_trees,\n" +
            "    Data = ImplMod:new(),\n" +
            "    #?MODULE{impl_mod=ImplMod, data=Data}.\n" +
            "\n" +
            "-spec add(key(), value(), ordmap()) -> ordmap() | no_return().\n" +
            "add(Key, Val, #?MODULE{impl_mod=ImplMod, data=Data}=Ordmap) ->\n" +
            "    Data1 = ImplMod:add(Key, Val, Data),\n" +
            "    Ordmap#?MODULE{data=Data1}."),

        prepareQuestion('Erlang', languages,
            "-module(fib).\n" +
            "-export([fibo/1, printfibo/1]).\n" +
            " \n" +
            "%% print fibo arg. and result, with function as parameter\n" +
            " \n" +
            "printfibo(N) -> \n" +
            "   Res = fib:fibo(N),\n" +
            "   io:fwrite(\"~w ~w~n\", [N, Res]).\n" +
            " \n" +
            "fibo(0) -> 0 ; \n" +
            "fibo(1) -> 1 ; \n" +
            "fibo(N) when N > 0 -> fibo(N-1) + fibo(N-2) ."),

        prepareQuestion('Elm', languages,
            "main =\n" +
            "  Browser.element\n" +
            "    { init = init\n" +
            "    , update = update\n" +
            "    , subscriptions = subscriptions\n" +
            "    , view = view\n" +
            "    }\n" +
            "\n" +
            "type alias Model =\n" +
            "  { dieFace : Int\n" +
            "  }\n" +
            "\n" +
            "init : () -> (Model, Cmd Msg)\n" +
            "init _ =\n" +
            "  ( Model 1\n" +
            "  , Cmd.none\n" +
            "  )\n" +
            "\n" +
            "type Msg\n" +
            "  = Roll\n" +
            "  | NewFace Int\n" +
            "\n" +
            "\n" +
            "update : Msg -> Model -> (Model, Cmd Msg)\n" +
            "update msg model =\n" +
            "  case msg of\n" +
            "    Roll ->\n" +
            "      ( model\n" +
            "      , Random.generate NewFace (Random.int 1 6)\n" +
            "      )\n" +
            "\n" +
            "    NewFace newFace ->\n" +
            "      ( Model newFace\n" +
            "      , Cmd.none\n" +
            "      )"),

        prepareQuestion('Elm', languages,
            "type alias Uniforms =\n" +
            "  { perspective : Mat4\n" +
            "  }\n" +
            "\n" +
            "\n" +
            "vertexShader : WebGL.Shader Vertex Uniforms { vcolor : Vec3 }\n" +
            "vertexShader =\n" +
            "    [glsl|\n" +
            "        attribute vec3 position;\n" +
            "        attribute vec3 color;\n" +
            "        uniform mat4 perspective;\n" +
            "        varying vec3 vcolor;\n" +
            "\n" +
            "        void main () {\n" +
            "            gl_Position = perspective * vec4(position, 1.0);\n" +
            "            vcolor = color;\n" +
            "        }\n" +
            "    |]\n" +
            "\n" +
            "\n" +
            "fragmentShader : WebGL.Shader {} Uniforms { vcolor : Vec3 }\n" +
            "fragmentShader =\n" +
            "    [glsl|\n" +
            "        precision mediump float;\n" +
            "        varying vec3 vcolor;\n" +
            "\n" +
            "        void main () {\n" +
            "            gl_FragColor = vec4(vcolor, 1.0);\n" +
            "        }\n" +
            "    |]"),

        prepareQuestion('Elm', languages,
            "type Msg\n" +
            "  = GotText (Result Http.Error String)\n" +
            "\n" +
            "\n" +
            "update : Msg -> Model -> (Model, Cmd Msg)\n" +
            "update msg model =\n" +
            "  case msg of\n" +
            "    GotText result ->\n" +
            "      case result of\n" +
            "        Ok fullText ->\n" +
            "          (Success fullText, Cmd.none)\n" +
            "\n" +
            "        Err _ ->\n" +
            "          (Failure, Cmd.none)\n" +
            "\n" +
            "subscriptions : Model -> Sub Msg\n" +
            "subscriptions model =\n" +
            "  Sub.none\n" +
            "\n" +
            "\n" +
            "view : Model -> Html Msg\n" +
            "view model =\n" +
            "  case model of\n" +
            "    Failure ->\n" +
            "      text \"I was unable to load your book.\"\n" +
            "\n" +
            "    Loading ->\n" +
            "      text \"Loading...\"\n" +
            "\n" +
            "    Success fullText ->\n" +
            "      pre [] [ text fullText ]"),

        prepareQuestion('Elixir', languages,
            "defmodule User do\n" +
            "  use Ecto.Schema\n" +
            "\n" +
            "  schema \"users\" do\n" +
            "    field :first_name, :string\n" +
            "    field :last_name,  :string\n" +
            "  end\n" +
            "\n" +
            "  def full_name(user) do\n" +
            "      user.first_name <> \" \" <> user.last_name\n" +
            "  end\n" +
            "\n" +
            "end"),

        prepareQuestion('Elixir', languages,
            "defmodule HelloWorld.Endpoint do\n" +
            "  use Phoenix.Endpoint, otp_app: :hello_world\n" +
            "\n" +
            "  plug Plug.Static,\n" +
            "    at: \"/\", from: :hello_world, gzip: false,\n" +
            "    only: ~w(css images js favicon.ico robots.txt)\n" +
            "\n" +
            "  if code_reloading? do\n" +
            "    plug Phoenix.LiveReloader\n" +
            "    plug Phoenix.CodeReloader\n" +
            "  end\n" +
            "\n" +
            "  plug Plug.Logger\n" +
            "\n" +
            "  plug Plug.Parsers,\n" +
            "    parsers: [:urlencoded, :multipart, :json],\n" +
            "    pass: [\"*/*\"],\n" +
            "    json_decoder: Poison\n" +
            "\n" +
            "  plug Plug.MethodOverride\n" +
            "  plug Plug.Head\n" +
            "\n" +
            "  plug Plug.Session,\n" +
            "    store: :cookie,\n" +
            "    key: \"_hello_world_key\",\n" +
            "    signing_salt: \"0yg9mHDO\"\n" +
            "\n" +
            "  plug :router, HelloWorld.Router\n" +
            "end"),

        prepareQuestion('Elixir', languages,
            "defmodule HelloWorld.Router do\n" +
            "  use HelloWorld.Web, :router\n" +
            "\n" +
            "  pipeline :browser do\n" +
            "    plug :accepts, [\"html\"]\n" +
            "    plug :fetch_session\n" +
            "    plug :fetch_flash\n" +
            "    plug :protect_from_forgery\n" +
            "  end\n" +
            "\n" +
            "  pipeline :api do\n" +
            "    plug :accepts, [\"json\"]\n" +
            "  end\n" +
            "\n" +
            "  scope \"/\", HelloWorld do\n" +
            "    pipe_through :browser # Use the default browser stack\n" +
            "\n" +
            "    get \"/\", PageController, :index\n" +
            "  end\n" +
            "\n" +
            "  # end\n" +
            "end"),
    ];
});
