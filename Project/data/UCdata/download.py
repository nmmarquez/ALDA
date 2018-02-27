"""Download UC Application Data."""
from selenium import webdriver
from selenium.webdriver.common.action_chains import ActionChains
import pandas as pd
import time
import itertools as it


class CollegeDownloader:
    """Download Data from the UC by school data platform."""

    def __init__(self):
        """Init."""
        self.url = (
            "https://visualizedata.ucop.edu/t/Public/views/"
            "AdmissionsDataTable/FroshbyYr?:embed=y&amp;:"
            "showVizHome=no&amp;:host_url=https%3A%2F%2Fvisualizedata.ucop.edu"
            "%2F&amp;:tabs=yes&amp;:toolbar=yes&amp;:showShareOptions=true&amp"
            ";:display_spinner=no&amp;:loadOrderID=0")
        self.uc_list = [
            "Berkeley", "Davis", "Irvine", "Los Angeles", "Merced",
            "Riverside", "San Diego", "Santa Barbara", "Santa Cruz"
            ]
        self.school_types = ["California Public High School",
                             "California Private High School"]
        self.driver = webdriver.Chrome()
        self.driver.implicitly_wait(10)
        self.driver.get(self.url)
        self.uc_dict = {x: i+1 for i, x in enumerate(self.uc_list)}
        self.years = list(reversed(range(1994, 2017)))
        self.year_dict = {x: i for i, x in enumerate(self.years)}
        self.school = ["Public", "Private"]
        self.school_dict = {x: i+1 for i, x in enumerate(self.school)}
        time.sleep(7)

    def inquire(self, uc, school, year):
        """Query data on page."""
        # Query the school
        js_script = ('document.getElementsByClassName('
                     '"tabComboBoxNameContainer")[0].click()')
        self.driver.execute_script(js_script)
        js_script = ('document.getElementsByClassName('
                     '"FIItem FISimpleDropdownItem")'
                     '[{}].click()'.format(self.uc_dict[uc]))
        self.driver.execute_script(js_script)
        # query the year
        js_script = ('document.getElementsByClassName('
                     '"tabComboBoxNameContainer")[1].click()')
        self.driver.execute_script(js_script)
        js_script = ('document.getElementsByClassName('
                     '"FIItem FISimpleDropdownItem")'
                     '[{}].click()'.format(self.year_dict[year]))
        self.driver.execute_script(js_script)
        # query the school type
        js_script = ('document.getElementsByClassName('
                     '"FICheckRadio")'
                     '[{}].click()'.format(self.school_dict[school]))
        self.driver.execute_script(js_script)
        time.sleep(2)

    def download(self):
        """Download Data."""
        img_search = "//div[@class='tvBackgroundContainer']"
        img_to_choose = self.driver.find_element_by_xpath(img_search)
        img_sel = ActionChains(self.driver).double_click(img_to_choose)
        img_sel.perform()
        time.sleep(5)
        element_to_choose = self.driver.find_element_by_xpath(
            "//div[@aria-label='Download']")
        hover = ActionChains(self.driver).click(element_to_choose)
        hover.perform()
        dl_search = "//div[@role='button' and @title='Data']"
        dl_button = self.driver.find_element_by_xpath(dl_search)
        ActionChains(self.driver).click(dl_button).perform()
        time.sleep(2)
        self.driver.switch_to_window(self.driver.window_handles[-1])
        csv_search = "//a[@class='csvLink_summary']"
        csv_button = self.driver.find_element_by_xpath(csv_search)
        df = pd.read_csv(csv_button.get_attribute("href"))
        self.driver.close()
        self.driver.switch_to_window(self.driver.window_handles[0])
        return df

    def process_download(self, uc, school, year):
        """Set up page and process download."""
        self.inquire(uc, school, year)
        df = self.download()
        df["UC"] = uc
        df["Year"] = year
        df["SchoolType"] = school
        f = "./{u}_{s}_{y}.csv".format(u=uc, s=school, y=year)
        print("Finished Processing: " + f)
        df.to_csv(f)

    def process_all(self):
        """Process all data."""
        for uc, s, y in it.product(self.uc_list, self.school, self.years):
            if uc == "Merced" and y < 2004:
                continue
            self.process_download(uc, s, y)


if __name__ == "__main__":
    self = CollegeDownloader()
    self.process_all()
