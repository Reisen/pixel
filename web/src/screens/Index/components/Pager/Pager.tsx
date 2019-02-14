import IconButton          from '../../../../components/IconButton';
import React, { useState } from 'react';
import styles              from './Pager.module.css';

interface Props {
    total: number;
}

const Pager = (props: Props) => {
    const [page, setPage] = useState(1);

    return (
        <div className={styles.Pager}>
            <div className={styles.IconRow}>
            {
                Array(7).fill(0).map((_, k) =>
                    <IconButton
                        active={(k + 1) === page}
                        icon={String(k + 1)}
                    />
                )
            }
            </div>

            <div className={styles.IconRow}>
                <IconButton icon="<" onClick={() => setPage(a => a - 1)} />
                <IconButton icon=">" onClick={() => setPage(a => a + 1)} />
            </div>
        </div>
    );
};

export default Pager;
