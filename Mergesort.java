public class Mergesort  
{
void merge(int tab[], int poczatek, int srodek, int koniec)
{

int l = srodek - poczatek +1;
int r = koniec - srodek;

int LeftArray[] = new int [l];
int RightArray[] = new int [r];

for (int i=0; i<l; ++i)  LeftArray[i] = tab[poczatek + i];

for (int j=0; j<r; ++j)  RightArray[j] = tab[srodek +1 +j];


int i = 0, j = 0;
int k = poczatek;
while (i<l&&j<r)
	{
		if (LeftArray[i] <= RightArray[j])
		{
			tab[k] = LeftArray[i];
			i++;
		}
		else
		{
			tab[k] = RightArray[j];
			j++;
		}
		k++;
	}
while (i<l)
	{
	tab[k] = LeftArray[i];
	i++;
	k++;
	}

while (j<r)
	{
	tab[k] = RightArray[j];
	j++;
	k++;
	}
}

void sort(int arr[], int beg, int end)
{
	if (beg<end)
		{
			int mid = (beg+end)/2;
			sort(arr, beg, mid);
			sort(arr , mid+1, end);
			merge(arr, beg, mid, end);
		}
}


}
