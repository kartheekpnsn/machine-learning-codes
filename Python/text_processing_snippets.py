import re

class TextPreprocessing:

    def __init__(self):
        """
        Initialization of TextPreprocessing class
        """
        pass
    
    @staticmethod
    def extract_all_numbers(text):
        return re.findall(r"[-+]?\d*\.\d+|\d+", text)

    @staticmethod
    def remove_multiple_line_spaces(text):
        """
        Removes multiple new line spaces
        :param text: string, free text
        :return: string, Processed text after removing multiple spaces
        """
        return "\n".join([line.strip() for line in text.splitlines() if line.strip() != ""])
    
    @staticmethod
    def remove_multiple_spaces(text):
        """
        Removes multiple spaces
        :param text: string, free text
        :return: string, Processed text after removing multiple spaces
        """
        return "\n".join([re.sub('\s+', ' ', line).strip() for line in text.splitlines()]).strip()

    @staticmethod
    def remove_escape_characters(text):
        """
        Removes the escape characters from the input text
        :param text: string, free text
        :return:
        - string, that has replaced escape characters
        - int, count of occurrence of escape characters
        """
        count = 0
        escapes = ''.join([chr(char) for char in range(1, 32)])
        text = text.translate(escapes)
        _tmp = re.sub(r'[\x00-\x1f\x7f-\x9f]', ' ', text)
        _tmp = TextPreprocessing.remove_multiple_spaces(_tmp)
        return _tmp, count

    @staticmethod
    def remove_unicode_characters(text):
        """
        Removes the unicode characters from the input text
        :param text: string, free text
        :return:
        - string, that has replaced escape characters
        - int, count of occurrence of unicode characters
        """
        count = 0
        _tmp = re.sub('([^\x00-\x7F])+', ' ', text)
        _tmp = TextPreprocessing.remove_multiple_spaces(_tmp)
        return _tmp, count

    @staticmethod
    def split_camel_case_text(text):
        """
        Splits text that has CamelCase into Camel Case (multiple words)
        :param text: string, free text - could be bug summary
        :return:
        - string, Processed text after splitting camel case words
        - boolean, whether the pattern existed in the text or not
        """
        matches = re.finditer(
            '.+?(?:(?<=[a-z])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])|$)', text)
        flag_matches = [match.group(0) for match in matches]
        count = len(flag_matches)
        print(flag_matches)
        flag = False  # whether it has camel case words or not
        if count > 0:
            flag = True  # whether it has camel case words or not
        words = re.sub('([A-Z][a-z]+)', r' \1', re.sub('([A-Z]+)', r' \1',
                                                       text)).split()
        _tmp = " ".join([word.strip() for word in words])
        _tmp = TextPreprocessing.remove_multiple_spaces(_tmp)
        return _tmp, flag

    @staticmethod
    def replace_symbols(text, f='&', t='and'):
        """
        Replaces the symbols with the mentioned text word.
        :param text: string, free text - could be bug summary
        :param f: string, from which symbol
        :param t: string, to which word
        :return:
        - string, Processed text after replacing input symbols with the mentioned text
        - int, Number of occurrences of such pattern
        - boolean, whether the pattern existed in the text or not
        """
        count = 0
        flag = False
        count = len([match.group(0) for match in re.finditer(f'({f})+', text)])
        if count > 0:
            flag = True
        count = text.count(f)
        _tmp = re.sub(f'({f})+', f" {t} ", text)
        _tmp = TextPreprocessing.remove_multiple_spaces(_tmp)
        return _tmp, count

    @staticmethod
    def extract_filename_from_path(x):
        """
        Extracts file name from the given path
        :param x: string, path from the free text data
        :return: string, the file name extracted from the filename
        """
        if "/" in x:
            return x.split("/")[-1]
        else:
            return x.split("\\.")[-1]

    @staticmethod
    def remove_paths(text):
        """
        Removes path from a given text
        :param text: string, free text - could be bug summary
        :return:
        - string, Processed text after path removal
        - boolean, whether the pattern existed in the text or not
        """
        flag = False
        count = len([
            match.group(0)
            for match in re.finditer(r'\s[/\\](?:(?!\.\s+)\S)+(\.)?', text)
        ])
        if count > 0:
            flag = True
        _tmp = re.sub(
            r'\s[/\\](?:(?!\.\s+)\S)+(\.)?', lambda x:
            f' {TextPreprocessing.extract_filename_from_path(x.group())}',
            text)
        _tmp = TextPreprocessing.remove_multiple_spaces(_tmp)
        return _tmp, flag

    @staticmethod
    def convert_case(text, which='lower'):
        """

        :param text:
        :param which:
        :return:
        """
        count = 0
        if which == 'lower':
            return text.lower(), count
        elif which == 'upper':
            return text.upper(), count
        else:
            return text, count
        
    @staticmethod
    def remove_stop_words(text, extra_words = []):
        extra_words = list(set([w.strip().lower() for w in extra_words]))
        STOPWORDS = list(set(stopwords.words('english')))
        STOPWORDS = set(STOPWORDS + extra_words)
        _tmp = []
        for line in text.splitlines():
            final_words = []
            for word in line.split():
                if word.strip().lower() not in STOPWORDS:
                    final_words.append(word.strip())
            _tmp.append(" ".join(final_words))
        _tmp = "\n".join(_tmp)
        _tmp = TextPreprocessing.remove_multiple_line_spaces(_tmp)
        _tmp = TextPreprocessing.remove_multiple_spaces(_tmp)
        return _tmp, list(STOPWORDS)

    @staticmethod
    def stem_the_text(text, use_lemmatizer=False):
        """

        :param text:
        :param use_lemmatizer:
        :return:
        """
        count = 0
        if use_lemmatizer:
            wl = WordNetLemmatizer()
            _tmp = " ".join([wl.lemmatize(word) for word in text.split()])
        else:
            ps = PorterStemmer()
            _tmp = " ".join([ps.stem(word) for word in text.split()])
        _tmp = TextPreprocessing.remove_multiple_spaces(_tmp)
        return _tmp, count

    @staticmethod
    def remove_html_tags(text):
        """
        Removes HTML tags and restoring text in between tags
        :param text: string, free text - could be bug summary
        :return:
        - string, Processed text after removing HTML tags and restoring text in between tags
        - boolean, whether the pattern existed in the text or not
        """
        flag = False
        text = text.replace("<br/>", "\n")  # newline character
        text = text.replace("<br>", "\n")  # newline character
        text = text.replace("h4.", "")  # Heading 4
        text = text.replace("h4", "")  # Heading 4
        html_filter = re.compile(
            '<.*?>|&([a-z0-9]+|#[0-9]{1,6}|#x[0-9a-f]{1,6});')
        _tmp = re.sub(html_filter, '', text)
        _tmp = re.sub(r"{color.{0,9}}", "\n", _tmp)
        _tmp = TextPreprocessing.remove_multiple_line_spaces(_tmp)
        _tmp = TextPreprocessing.remove_multiple_spaces(_tmp)
        return _tmp, flag

    @staticmethod
    def replace_acronyms(acronyms_dict, text):
        """
        Replaces the acronyms in a text with full text
        :param acronyms_dict: dictionary, key as acronym shortcut and value as fullform
        :param text: string, free text - could be bug summary
        :return:
        - string, Processed text after replacing acronyms
        - boolean, whether the pattern existed in the text or not
        """
        flag = False
        for acronym in sorted(acronyms_dict, key=len, reverse=True):
            if acronym in text:
                if len(acronym.split()) == 1:
                    # if it is a single word
                    if acronym in text.split():
                        # if the single word is found as a word in text
                        flag = True
                        text = text.replace(acronym,
                                            acronyms_dict[acronym])
                else:
                    # if the acronym is more than a word, direct replace
                    flag = True
                    text = text.replace(acronym, acronyms_dict[acronym])
        _tmp = TextPreprocessing.remove_multiple_spaces(text)
        return _tmp, flag

    @staticmethod
    def remove_punctuations(text, punct_str=''):
        """
        Replaces the Punctuations from a given text
        :param text: string, free text - could be bug summary
        :param punct_str: string, which all punctuation symbols to be replaced
        :return:
        - string, Processed text after replacing punctuations.
        - int, count of the pattern existence
        """
        count = 0
        if punct_str == "":
            punct_str = string.punctuation
        for punctuation in punct_str:
            text = text.replace(punctuation, ' ')
        _tmp = TextPreprocessing.remove_multiple_line_spaces(text)
        _tmp = TextPreprocessing.remove_multiple_spaces(_tmp)
        return _tmp, count
    
    @staticmethod
    def get_all_acronym_words(text):
        rx = r"\b[A-Z](?=([&.]?))(?:\1[A-Z])+\b"
        return [x.group() for x in re.finditer(rx, text)]

    @staticmethod
    def get_rare_words(corpus, threshold=0.9):
        """

        :param corpus:
        :param threshold:
        :return:
        """
        corpus = TextPreprocessing.remove_punctuations(text=corpus)[0].lower()
        tokens = corpus.split()
        freq_dist = nltk.FreqDist(tokens)
        freq_dist_pd = pd.DataFrame({'Word': list(freq_dist.keys()), 'Frequency': list(freq_dist.values())})
        freq_dist_pd = freq_dist_pd.sort_values('Frequency', ascending=False)
        freq_dist_pd['CumulativePercentage'] = (freq_dist_pd.Frequency.cumsum() / freq_dist_pd.Frequency.sum())
        rare_words = freq_dist_pd[freq_dist_pd['CumulativePercentage'] > threshold]['Word'].tolist()
        return rare_words
    
    @staticmethod
    def get_frequent_words(corpus, threshold=0.9):
        """

        :param corpus:
        :param threshold:
        :return:
        """
        corpus = TextPreprocessing.remove_punctuations(text=corpus)[0].lower()
        tokens = corpus.split()
        freq_dist = nltk.FreqDist(tokens)
        freq_dist_pd = pd.DataFrame({'Word': list(freq_dist.keys()), 'Frequency': list(freq_dist.values())})
        freq_dist_pd = freq_dist_pd.sort_values('Frequency', ascending=True)
        freq_dist_pd['CumulativePercentage'] = (freq_dist_pd.Frequency.cumsum() / freq_dist_pd.Frequency.sum())
        freq_words = freq_dist_pd[freq_dist_pd['CumulativePercentage'] > threshold]['Word'].tolist()
        return freq_words
    
    @staticmethod
    def remove_email_ids(text):
        emails = re.findall(r"[a-z0-9\.\-+_]+@[a-z0-9\.\-+_]+\.[a-z]+", text)
        _tmp = TextPreprocessing.remove_multiple_spaces(text)
        for idx in emails:
            _tmp = _tmp.replace(idx, "")
        _tmp = TextPreprocessing.remove_multiple_spaces(_tmp)
        return _tmp, emails
            
    
    @staticmethod
    def remove_urls(text):
        url_pattern = re.compile(r'https?://\S+|www\.\S+')
        _tmp = url_pattern.sub(r'', text)
        _tmp = TextPreprocessing.remove_multiple_spaces(_tmp)
        return _tmp
    
    @staticmethod
    def remove_ip_address(text):
        # declaring the regex pattern for IP addresses
        pattern = re.compile(r'(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})')

        ip_address_list=[]
        for line in text.splitlines():
            search_obj = re.findall(pattern, line)
            if search_obj:
                ip_address_list.extend(search_obj)
            
        for ip_add in ip_address_list:
            text = text.replace(ip_add, "")
        return text, ip_address_list
        
    
    @staticmethod
    def remove_frequent_words(text, corpus, threshold=0.9):
        """

        :param text:
        :param corpus:
        :param threshold:
        :return:
        """
        count = 0
        if isinstance(corpus, list):
            print("Joining the corpus")
            corpus = " ".join(corpus)
            corpus = TextPreprocessing.remove_multiple_spaces(corpus)
        rare_words = TextPreprocessing.get_frequent_words(corpus=corpus, threshold=threshold)
        _tmp = " ".join(
            [w for w in text.split() if TextPreprocessing.remove_punctuations(w)[0].lower() not in rare_words])
        _tmp = TextPreprocessing.remove_multiple_spaces(_tmp)
        return _tmp, count

    @staticmethod
    def remove_rare_words(text, corpus, threshold=0.9):
        """

        :param text:
        :param corpus:
        :param threshold:
        :return:
        """
        count = 0
        if isinstance(corpus, list):
            print("Joining the corpus")
            corpus = " ".join(corpus)
            corpus = TextPreprocessing.remove_multiple_spaces(corpus)
        rare_words = TextPreprocessing.get_rare_words(corpus=corpus, threshold=threshold)
        _tmp = " ".join(
            [w for w in text.split() if TextPreprocessing.remove_punctuations(w)[0].lower() not in rare_words])
        _tmp = TextPreprocessing.remove_multiple_spaces(_tmp)
        return _tmp, count
