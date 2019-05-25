import os
os.chdir("d:\\drobyshev\\Prog\\mylibs")
from reader import*
from sketch import*

class TStend():
    def __init__(self, MAIN_DIR):
        self.MAIN_DIR = MAIN_DIR # директория проекта
        self.sk = TSketch(MAIN_DIR)   # для компиляции и запуска программы sketch
        self.rd = TReader() # для чтения файлов
        pass
    def changefile(self,templatename, filename, par):
        """Запись файла с новыми параметрами"""
        # открытие файла-шаблона
        f = open(self.MAIN_DIR+"\\pymodul\\template\\" + templatename,"r")
        template = f.read()
        f.close()
        # запись файла с новыми параметрами
        f = open(self.MAIN_DIR + "\\input\\" + filename,"w")
        #f.write(template%(par))
        f.write(template)
        f.close()
        pass
    def calc(self, time = 1.0, mode = "ss"):
        """Расчет проекта"""
        if mode == "ss":
            # устанавливаем начальное положение стержней в stend_3.ini
            self.changefile("BN800.dat","STEND_3.dat",(9,9))
            # устанавливаем параметры стационарного расчета в sketch.ini
            self.changefile("#Sketch_ss.ini","Sketch.ini",())
            # запускаем расчет стационарного состояния
            self.sk.run("sketch")
        if mode == "kin":
            # устанавливаем параметры расчета кинетики в sketch.ini
            self.changefile("#Sketch_kin.ini","Sketch.ini",())
            # устанавливаем новое положение стержней в stend_3.ini
            self.changefile("#Stend.dat","Stend_3.dat",())
            # запускаем нестационарный расчет
            self.sk.run("sketch")
        pass
    def getf(self,time):
        """Получить поле в данный момент времени"""
        self.rd.base = []
        # новое время в файле PostProc.ini
        self.changefile("#PostProc.ini","PostProc.ini",())
        #  устанавливаем указатель на поле нейтронов 1-й группы
        self.changefile("#PP_STEND.dat","PP_STEND.dat",())
        # запускаем постпроцессор
        self.sk.run("postproc")
        self.rd.read([["sketch",self.MAIN_DIR+"\\Output\\PP_STEND.lst"]])
        #  устанавливаем указатель на поле нейтронов 2-й группы
        self.changefile("#PP_STEND.dat","PP_STEND.dat",())
        # запускаем постпроцессор
        self.sk.run("postproc")
        self.rd.read([["sketch",self.MAIN_DIR+"\\Output\\PP_STEND.lst"]])
        return np.array(self.rd.base)