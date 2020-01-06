#include <iostream>
using namespace std;
struct node {
    int num;
    node *link;
};
int main() {
    node *head, *tail, *p;
    int d, i;
    head = NULL;
    for (i = 0; i < 10; i++) {
        p = new node;
        cin >> p->num;
        if (head == NULL)
            head = p;
        else
            tail->link = p;
        tail = p;
    }
    tail->link = NULL;
    cin >> d;
    p = head;
    i = 0;
    while (p != NULL) {
        i++;
        if (p->num == d)
            break;
        else
            p = p->link;
    }
    if (p == NULL)
        cout << d << "不在该链表中";
    else
        cout << d << "为该链表中的第" << i << "个结点";
    // system("pause");
    return 0;
}
// 1 2 3 4 5 6 7 8 9 10
