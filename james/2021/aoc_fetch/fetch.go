package aoc_fetch

import (
	"crypto/tls"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"os"
)

func addCookie(req *http.Request) error {
	if token, ok := os.LookupEnv("AOC_TOKEN"); ok {
		req.AddCookie(&http.Cookie{
			Name:  "session",
			Value: token,
		})
		return nil
	} else {
		return fmt.Errorf("Set AOC_TOKEN to fetch input")
	}
}

func cacheFilepath(year, day string) (string, error) {
	user_cache_dir, err := os.UserCacheDir()

	if err != nil {
		return "", fmt.Errorf("Failed to determine cache path: %v", err)
	}

	file := fmt.Sprintf("%s/advent-of-code-input-%s-%s", user_cache_dir, year, day)
	return file, err
}

func checkCache(year, day string) ([]byte, error) {
	cache_path, err := cacheFilepath(year, day)

	if err != nil {
		return nil, err
	}

	contents, err := ioutil.ReadFile(cache_path)
	return contents, err
}

func cacheInput(year, day string, contents []byte) error {
	cache_path, err := cacheFilepath(year, day)

	if err != nil {
		return err
	}

	return ioutil.WriteFile(cache_path, contents, 0444)
}

func fetchInput(year, day string) ([]byte, error) {
	client := &http.Client{
		Transport: &http.Transport{
			TLSClientConfig: &tls.Config{},
		},
	}

	url := fmt.Sprintf("https://adventofcode.com/%s/day/%s/input", year, day)
	req, err := http.NewRequest("GET", url, nil)

	if err != nil {
		return nil, err
	}

	if err = addCookie(req); err != nil {
		return nil, err
	}

	resp, err := client.Do(req)
	if err != nil {
		return nil, err
	}

	contents, err := io.ReadAll(resp.Body)
	return contents, err
}

func PuzzleInputFor(year, day string) ([]byte, error) {
	contents, err := checkCache(year, day)

	if err == nil {
		fmt.Printf("Found cached input for %v %v\n", year, day)
		return contents, nil
	} else {
		fmt.Printf("No cached version found, will try to fetch: %v\n", err)
	}

	contents, err = fetchInput(year, day)

	if err != nil {
		return nil, err
	}

	if err = cacheInput(year, day, contents); err != nil {
		return nil, err
	}

	return contents, nil
}
